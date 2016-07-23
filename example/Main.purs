module Main where

import Prelude
import Database.Neo4J
import Control.Monad.Aff (Aff, attempt, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Either (Either(..))
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric)
import Data.Generic (class Generic, gEq, gShow)

-- | The information used to connect to the server.
connectionInfo :: ConnectionInfo
connectionInfo = ConnectionInfo
  { url: "bolt://localhost"
  , auth: mkAuth "neo4j" "password4test"
  , connectionOpts: defaultConnectionOptions
  }

-- | Type for the database result.
newtype Person = Person
  { name :: String
  , title :: String
  }

derive instance genericPerson :: Generic Person

instance isForeignPerson :: IsForeign Person where
  read = readGeneric defaultForeignOpts

instance eqPerson :: Eq Person where
  eq = gEq

instance showPerson :: Show Person where
  show = gShow

-- | Add a record to the DB and return it.
addToDB :: forall eff. Aff (neo4j :: NEO4J | eff) (Array Person)
addToDB =
  withDriver connectionInfo $ \driver ->
    withSession driver $ \session ->
      withRollback session $ do
        execute' (Query "CREATE (a:Person {name:'Arthur', title:'King'})")
        query (Query "MATCH (a:Person) WHERE a.name = {name} RETURN a" :: Query Person) (mkParams {name: "Arthur"})

expected :: Array Person
expected = [Person { name: "Arthur", title: "King"}]

log' :: forall eff. String -> Aff (console :: CONSOLE | eff) Unit
log' x = liftEff (log x)

logShow' :: forall eff a. (Show a) => a -> Aff (console :: CONSOLE | eff) a
logShow' x = do
  liftEff (logShow x)
  pure x

main :: Eff (console :: CONSOLE, err :: EXCEPTION, neo4j :: NEO4J) Unit
main = void $ launchAff do
  results <- attempt addToDB
  case results of
    Left err -> logShow' err *> pure unit
    Right results' ->
      if results' == expected
      then log' "Ok"
      else do
        log' "Results did not match"
        log' "Expected:"
        logShow' expected
        log' "Actual:"
        logShow' results'
        pure unit
