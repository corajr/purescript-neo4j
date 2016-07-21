module Test.Neo4J where

import Prelude

import Test.Spec (describe, pending, it)
import Test.Spec.Assertions (shouldEqual)
import Test.QuickCheck ((===))
import Test.Spec.QuickCheck (quickCheck)

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
  describe "integration" do
    describe "withConnection" do
      it "wraps a database query in a session, closing the connection when finished" do
        let info = ConnectionInfo { url: "bolt://localhost"
                                  , auth: mkAuth "neo4j" "password4test"
                                  , connectionOpts: defaultConnectionOptions
                                  }
        personResults <- withConnection info $ \session -> do
          execute' (Query "CREATE (a:Person {name:'Arthur', title:'King'})") session
          query (Query "MATCH (a:Person) WHERE a.name = {name} RETURN a" :: Query Person) (mkParams {name: "Arthur"}) session
        personResults `shouldEqual` [Person { name: "Arthur", title: "King" }]
