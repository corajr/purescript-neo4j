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
newtype Track = Track
  { id :: Int
  , title :: String
  }

derive instance genericTrack :: Generic Track

instance eqTrack :: Eq Track where
  eq = gEq

instance showTrack :: Show Track where
  show = gShow

instance isForeignTrack :: IsForeign Track where
  read = readGeneric defaultForeignOptions

-- | Add a record to the DB and return it.
addToDB :: forall eff. Session -> Aff (neo4j :: NEO4J | eff) (Array Track)
addToDB session =
  withRollback session $ do
    execute (Query "CREATE (a:Track {id: {id}, title: {title}})") (mkParams {id: 999, title: "A test"})
    query (Query "MATCH (a:Track) WHERE a.id = {id} RETURN a" :: Query Track) (mkParams {id: 999})

readFromDB :: forall eff. Session -> Aff (neo4j :: NEO4J | eff) (Array Track)
readFromDB session =
  withTransaction session $
    query (Query "MATCH (a:Track) WHERE a.id = {id} RETURN a" :: Query Track) (mkParams {id: 106521709})

expected :: Array Track
expected = [Track {id: 999, title: "A test"}]

log' :: forall eff. String -> Aff (console :: CONSOLE | eff) Unit
log' x = liftEff (log x)

logShow' :: forall eff a. (Show a) => a -> Aff (console :: CONSOLE | eff) a
logShow' x = do
  liftEff (logShow x)
  pure x

main :: Eff (console :: CONSOLE, err :: EXCEPTION, neo4j :: NEO4J) Unit
main = void $ launchAff do
  withDriver connectionInfo $ \driver ->
    withSession driver $ \session -> do
      results <- attempt (addToDB session)
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
      existing <- attempt (readFromDB session)
      logShow' existing
      pure unit
