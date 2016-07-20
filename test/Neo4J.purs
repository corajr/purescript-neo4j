module Test.Neo4J where

import Control.Monad.Aff

import Test.Spec (describe, pending, it)
import Test.Spec.Assertions (shouldEqual)

main = do
  describe "integration" do
    pending "withConnection"
