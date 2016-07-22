module Test.Neo4J where

import Prelude

import Test.Spec (describe, pending, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Assertions.Aff (expectError)
import Test.QuickCheck ((===))
import Test.Spec.QuickCheck (quickCheck)

import Data.Either (Either(..))
import Control.Monad.Aff (attempt)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)

import Database.Neo4J
import Test.Fixture

main = do
  describe "mkAuth" do
    it "takes a username and password and returns an Auth" $
      quickCheck \username pass ->
        mkAuth username pass === BasicAuth { scheme: "basic"
                                           , principal: username
                                           , credentials: pass
                                           }
  let info = ConnectionInfo { url: "bolt://localhost"
                            , auth: mkAuth "neo4j" "password4test"
                            , connectionOpts: defaultConnectionOptions
                            }
  describe "integration" do
    describe "withRollback" do
      it "wraps a database query in a transaction, closing the connection when finished" do
        personResults <- withDriver info $ \driver ->
          withSession driver $ \session -> do
            withRollback session $ do
              execute' (Query "CREATE (a:Person {name:'Arthur', title:'King'})")
              query (Query "MATCH (a:Person) WHERE a.name = {name} RETURN a" :: Query Person) (mkParams {name: "Arthur"})
        personResults `shouldEqual` [Person { name: "Arthur", title: "King" }]
