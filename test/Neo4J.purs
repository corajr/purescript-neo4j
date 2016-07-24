module Test.Neo4J where

import Prelude
import Database.Neo4J
import Test.Fixture
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Data.Either (Either(..))
import Test.QuickCheck ((===))
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

main = do
  describe "mkAuth" do
    it "takes a username and password and returns an Auth" $
      quickCheck \username pass ->
        mkAuth username pass === BasicAuth { scheme: "basic"
                                           , principal: username
                                           , credentials: pass
                                           }
  describe "toNeoInt" do
    it "fromNeoInt <<< toNeoInt == id for 32-bit ints" do
      quickCheck \i -> fromNeoInt (toNeoInt i) === i
  describe "integration" do
    describe "execute" do
      it "creates nodes and relationships" do
        relations <- runWithRollback do
          execute' (Query "CREATE (adam:User { name: 'Adam' }),(pernilla:User { name: 'Pernilla' }),(david:User { name: 'David'}), (adam)-[:FRIEND]->(pernilla),(pernilla)-[:FRIEND]->(david)")
          query' (Query "Match (a)-[x:FRIEND]->(b) RETURN x" :: Query' Relationship')
        map ((\(Relationship rec) -> rec."type") <<< unbox) relations `shouldEqual` ["FRIEND", "FRIEND"]
    describe "query" do
      it "returns an array of nodes" do
        results <- runWithRollback do
          query' (Query (exampleCreateQuery <> " RETURN a as x") :: Query' (Node Person))
        map (\(XRecord {x: (Node rec)}) -> rec.properties) results `shouldEqual` [examplePerson]
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
        map (\(XRecord {x: (Node rec)}) -> rec.properties) personResults `shouldEqual` [examplePerson]
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
