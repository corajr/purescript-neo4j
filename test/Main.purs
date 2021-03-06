module Test.Main where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Node.Process (PROCESS)
import Database.Neo4J (NEO4J)

import Test.Neo4J as N4J

import Test.Spec.Runner (run)
import Test.Spec.Reporter.Console (consoleReporter)

main :: forall t. Eff ( process :: PROCESS
                      , console :: CONSOLE
                      , random :: RANDOM
                      , neo4j :: NEO4J
                      | t) Unit
main = run [consoleReporter] do
  N4J.main
