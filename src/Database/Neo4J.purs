module Database.Neo4J where

import Prelude
import Data.Foreign (F, Foreign)
import Data.Foreign.Generic (Options(..), defaultOptions, readGeneric, toForeignGeneric)
import Data.Foreign.Class (class IsForeign, read)

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


mkAuth :: Username -> Password -> F BasicAuth
mkAuth user pass = read (mkAuth_ user pass)

foreign import mkAuth_ :: Username -> Password -> Foreign
