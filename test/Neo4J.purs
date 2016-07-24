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

exampleCreateQuery :: Query Unit
exampleCreateQuery = Query "CREATE (a:Person {name:'Arthur', age: 123})"

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
    describe "query" do
      it "returns an array of nodes" do
        results <- runWithRollback do
          execute' exampleCreateQuery
          query' (Query "MATCH (a:Person) RETURN a" :: Query NodePersonRec)
        map (\(NodePersonRec {a: (Node rec)}) -> rec.properties) results `shouldEqual` [examplePerson]
      it "can return a single string value" do
        results <- runWithRollback do
          execute' exampleCreateQuery
          query' (Query "MATCH (a:Person) RETURN a.name" :: Query NameRec)
        results `shouldEqual` [NameRec {"a.name": "Arthur"}]
      it "can return a single int" do
        results <- runWithRollback do
          execute' exampleCreateQuery
          query' (Query "MATCH (a:Person) RETURN a.age" :: Query AgeRec)
        results `shouldEqual` [AgeRec {"a.age": toNeoInt 123}]
    describe "withRollback" do
      it "wraps a database query in a transaction, closing the connection when finished" do
        personResults <- runWithRollback do
          execute' exampleCreateQuery
          query (Query "MATCH (a:Person) WHERE a.name = {name} RETURN a" :: Query NodePersonRec) (mkParams {name: "Arthur"})
        map (\(NodePersonRec {a: (Node rec)}) -> rec.properties) personResults `shouldEqual` [examplePerson]
      it "doesn't commit the results of the transaction" do
        { result1, result2 } <- withDriver serverInfo $ \driver ->
          withSession driver $ \session -> do
            a <- withRollback session $ do
              execute' exampleCreateQuery
              query (Query "MATCH (a:Person) WHERE a.name = {name} RETURN a" :: Query NodePersonRec) (mkParams {name: "Arthur"})
            b <- withRollback session $ do
              query (Query "MATCH (a:Person) WHERE a.name = {name} RETURN a" :: Query NodePersonRec) (mkParams {name: "Arthur"})
            pure { result1: a, result2: b }
        map (\(NodePersonRec {a: (Node rec)}) -> rec.properties) result1 `shouldEqual` [examplePerson]
        result2 `shouldEqual` []
