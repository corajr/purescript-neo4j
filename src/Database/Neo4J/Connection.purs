module Database.Neo4J.Connection where

import Prelude
import Data.Foreign (Foreign, toForeign, unsafeFromForeign, ForeignError)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (toForeignGeneric, Options, defaultOptions, readGeneric)
import Data.Generic (class Generic, gShow)

import Database.Neo4J.Types (defaultForeignOptions)

type Username = String
type Password = String

newtype BasicAuth = BasicAuth
  { scheme :: String
  , principal :: Username
  , credentials :: Password
  }

derive instance genericBasicAuth :: Generic BasicAuth

derive instance eqBasicAuth :: Eq BasicAuth

instance showBasicAuth :: Show BasicAuth where
  show = gShow

instance isForeignBasicAuth :: IsForeign BasicAuth where
  read = readGeneric defaultForeignOptions

newtype ConnectionOptions = ConnectionOptions
  { encrypted :: Boolean
  }

derive instance genericConnectionOpts :: Generic ConnectionOptions
derive instance eqConnectionOpts :: Eq ConnectionOptions

instance showConnectionOpts :: Show ConnectionOptions where
  show = gShow

instance isForeignConnectionOpts :: IsForeign ConnectionOptions where
  read = readGeneric defaultForeignOptions

defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
  { encrypted: false
  }

-- | Server connection info; example:
-- |
-- | { url: "bolt://localhost",
-- | , auth: mkAuth "username" "password"
-- | , connectionOpts: defaultConnectionOptions { encrypted = true }
-- | }
newtype ConnectionInfo = ConnectionInfo
  { url :: String
  , auth :: BasicAuth
  , connectionOpts :: ConnectionOptions
  }

derive instance genericConnectionInfo :: Generic ConnectionInfo
derive instance eqConnectionInfo :: Eq ConnectionInfo

instance showConnectionInfo :: Show ConnectionInfo where
  show = gShow

-- | Make a `BasicAuth` object suitable for the Neo4J driver.
mkAuth :: Username -> Password -> BasicAuth
mkAuth username password = BasicAuth
  { scheme: "basic"
  , principal: username
  , credentials: password
  }


