module Database.Neo4J where

import Prelude
import Data.Foreign (F)
import Data.Foreign.Generic (Options(..), defaultOptions, readGeneric)
import Data.Foreign.Class (class IsForeign)

import Data.Generic

import Control.Monad.Aff

foreign import data Connection :: *
foreign import data DB :: !

type Username = String
type Password = String

newtype BasicAuth = BasicAuth
  { scheme :: String
  , principal :: Username
  , credentials :: Password
  }

derive instance genericBasicAuth :: Generic BasicAuth

instance showBasicAuth :: Show BasicAuth where
  show = gShow

instance eqBasicAuth :: Eq BasicAuth where
  eq = gEq

myOptions :: Options
myOptions = defaultOptions { unwrapNewtypes = true }

instance isForeignBasicAuth :: IsForeign BasicAuth where
  read = readGeneric myOptions

data ConnectionInfo = ConnectionInfo
  { url :: String
  , auth :: BasicAuth
  }

foreign import mkAuth :: Username -> Password -> F BasicAuth
