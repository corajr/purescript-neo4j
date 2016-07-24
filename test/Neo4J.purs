module Test.Neo4J where

import Prelude
import Database.Neo4J
import Test.Fixture
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Array (take)
import Data.Either (Either(..))
import Test.QuickCheck ((===))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.Spec (describe, pending, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Assertions.Aff (expectError)
import Test.Spec.QuickCheck (quickCheck)

serverInfo :: ConnectionInfo
serverInfo = ConnectionInfo { url: "bolt://localhost"
                            , auth: mkAuth "neo4j" "password4test"
                            , connectionOpts: defaultConnectionOptions
                            }

runWithRollback f =
  withDriver serverInfo $ \driver ->
    withSession driver $ \session ->
      withRollback session $ f

examplePerson :: Person
examplePerson = Person { name: "Arthur", age: toNeoInt 123}

exampleCreateQuery :: String
exampleCreateQuery = "CREATE (a:Person {name:'Arthur', age: 123})"

newtype NInt = NInt NeoInteger

derive instance eqNInt :: Eq NInt

instance arbNeoInt :: Arbitrary NInt where
  arbitrary = do
    n <- arbitrary
    pure $ NInt (toNeoInt n)

main = do
  describe "mkAuth" do
    it "takes a username and password and returns an Auth" $
      quickCheck \username pass ->
        mkAuth username pass === BasicAuth { scheme: "basic"
                                           , principal: username
                                           , credentials: pass
                                           }
  describe "toNeoInt" do
    it "unsafeFromNeoInt <<< toNeoInt == id for 32-bit ints" do
      quickCheck \i -> unsafeFromNeoInt (toNeoInt i) === i
  describe "unsafeFromNeoInt" do
    it "toNeoInt <<< unsafeFromNeoInt == id for 32-bit ints" do
      quickCheck \(NInt i) -> toNeoInt (unsafeFromNeoInt i) === i
  describe "integration" do
    describe "execute" do
      it "takes parameters and creates nodes" do
        let nodeProps = [Person {name: "Angela", age: toNeoInt 62}, Person {name: "Beryl", age: toNeoInt 20}]
        nodes <- runWithRollback do
          execute (Query "UNWIND {nodeProps} AS properties CREATE (n:Person) SET n = properties") (mkParams {nodeProps: nodeProps})
          query' (Query "MATCH (x:Person) RETURN x ORDER BY x.name" :: Query' (Node Person))
        map (getProperties <<< unbox) nodes `shouldEqual` nodeProps
    describe "execute'" do
      it "takes no parameters and creates nodes and relationships" do
        relations <- runWithRollback do
          execute' (Query "CREATE (adam:User { name: 'Adam' }),(pernilla:User { name: 'Pernilla' }),(david:User { name: 'David'}), (adam)-[:FRIEND]->(pernilla),(pernilla)-[:FRIEND]->(david)")
          query' (Query "Match (a)-[x:FRIEND]->(b) RETURN x" :: Query' Relationship')
        map ((\(Relationship rec) -> rec."type") <<< unbox) relations `shouldEqual` ["FRIEND", "FRIEND"]
    describe "query" do
      it "takes parameters and returns an array of records" do
        let nodeProps = [ Person {name: "Angela", age: toNeoInt 62}
                        , Person {name: "Beryl", age: toNeoInt 20}
                        , Person {name: "Charlene", age: toNeoInt 10}
                        ]
        nodes <- runWithRollback do
          execute (Query "UNWIND {nodeProps} AS properties CREATE (n:Person) SET n = properties") (mkParams {nodeProps: nodeProps})
          query (Query "MATCH (x:Person) WHERE x.age > {age} RETURN x ORDER BY x.name" :: Query' (Node Person)) (mkParams {age:10})
        map (getProperties <<< unbox) nodes `shouldEqual` take 2 nodeProps
    describe "query'" do
      it "takes no parameters and returns an array of records" do
        results <- runWithRollback do
          query' (Query (exampleCreateQuery <> " RETURN a as x") :: Query' (Node Person))
        map (getProperties <<< unbox) results `shouldEqual` [examplePerson]
      it "can return a single string value" do
        results <- runWithRollback do
          query' (Query (exampleCreateQuery <> " RETURN a.name") :: Query NameRec)
        results `shouldEqual` [NameRec {"a.name": "Arthur"}]
      it "can return a single int" do
        results <- runWithRollback do
          query' (Query (exampleCreateQuery <> " RETURN a.age as x") :: Query' NeoInteger)
        results `shouldEqual` [XRecord {x: toNeoInt 123}]
    describe "withRollback" do
      it "wraps a database query in a transaction, closing the connection when finished" do
        personResults <- runWithRollback do
          execute' (Query exampleCreateQuery)
          query (Query "MATCH (x:Person) WHERE x.name = {name} RETURN x" :: Query' (Node Person)) (mkParams {name: "Arthur"})
        map (getProperties <<< unbox) personResults `shouldEqual` [examplePerson]
      it "doesn't commit the results of the transaction" do
        { result1, result2 } <- withDriver serverInfo $ \driver ->
          withSession driver $ \session -> do
            a <- withRollback session $ do
              execute' (Query exampleCreateQuery)
              query (Query "MATCH (x:Person) WHERE x.name = {name} RETURN x" :: Query' (Node Person)) (mkParams {name: "Arthur"})
            b <- withRollback session $ do
              query (Query "MATCH (x:Person) WHERE x.name = {name} RETURN x" :: Query' (Node Person)) (mkParams {name: "Arthur"})
            pure { result1: a, result2: b }
        map (\(XRecord {x: (Node rec)}) -> rec.properties) result1 `shouldEqual` [examplePerson]
        result2 `shouldEqual` []
