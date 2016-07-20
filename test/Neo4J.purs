module Test.Neo4J where

import Prelude
import Control.Monad.Aff

import Test.Spec (describe, pending, it)
import Test.Spec.Assertions (shouldEqual)
import Test.QuickCheck ((===))
import Test.Spec.QuickCheck (quickCheck)

import Database.Neo4J

main = do
  describe "mkAuth" do
    it "takes a username and password and returns an Auth" $
      quickCheck \username pass ->
        mkAuth username pass === pure (BasicAuth {scheme: "basic",
                                                  principal: username,
                                                  credentials: pass})
  describe "integration" do
    pending "withConnection"
