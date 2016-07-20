module Test.Main where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Node.Process (PROCESS)

import Test.Neo4J as N4J

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

main :: forall t. Eff ( process :: PROCESS , console :: CONSOLE | t) Unit
main = run [consoleReporter] do
  N4J.main
