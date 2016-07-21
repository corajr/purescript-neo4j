module Database.Neo4J where

import Prelude
import Data.Function.Uncurried
import Control.Monad.Aff (Aff, makeAff, finally, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Data.Either (either)
import Data.Foreign (F, Foreign, toForeign, unsafeFromForeign, ForeignError)
import Data.Foreign.Class (class IsForeign, read)
import Data.Foreign.Generic (toForeignGeneric, Options, defaultOptions, readGeneric)
import Data.Generic (class Generic, gEq, gShow)
import Data.Traversable (traverse)

foreign import data Driver :: *
foreign import data Session :: *
foreign import data Transaction :: *
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

newtype Query a = Query String

instance eqQuery :: Eq (Query a) where
  eq (Query a) (Query b) = a == b
instance showQuery :: Show (Query a) where
  show (Query n) = n

newtype Params = Params Foreign

myForeignOpts :: Options
myForeignOpts = defaultOptions { unwrapNewtypes = true }

instance isForeignBasicAuth :: IsForeign BasicAuth where
  read = readGeneric myForeignOpts

newtype ConnectionOptions = ConnectionOptions
  { encrypted :: Boolean
  }

derive instance genericConnectionOpts :: Generic ConnectionOptions

instance isForeignConnectionOpts :: IsForeign ConnectionOptions where
  read = readGeneric myForeignOpts

defaultConnectionOptions :: ConnectionOptions
defaultConnectionOptions = ConnectionOptions
  { encrypted: true
  }

data ConnectionInfo = ConnectionInfo
  { url :: String
  , auth :: BasicAuth
  , connectionOpts :: ConnectionOptions
  }

mkAuth :: Username -> Password -> BasicAuth
mkAuth user pass = unsafeFromForeign (mkAuth_ user pass)

mkDriver :: forall eff. ConnectionInfo -> Eff (db :: DB | eff) Driver
mkDriver (ConnectionInfo { url, auth, connectionOpts }) =
  let auth' = toForeignGeneric myForeignOpts auth
      connectionOpts' = toForeignGeneric myForeignOpts connectionOpts
  in runFn3 mkDriver_ url auth' connectionOpts'

mkSession :: forall eff. Driver -> Eff (db :: DB | eff) Session
mkSession driver = runFn1 mkSession_ driver

mkParams :: forall a. a -> Params
mkParams = Params <<< toForeign

withConnection :: forall eff a.
                  ConnectionInfo
               -> (Session -> Aff (db :: DB | eff) a)
               -> Aff (db :: DB | eff) a
withConnection info f = do
  driver <- liftEff $ mkDriver info
  session <- liftEff $ mkSession driver
  finally (f session) $ closeSession session *> closeDriver driver

execute :: forall eff. Query Unit -> Params -> Session -> Aff (db :: DB | eff) Unit
execute q params session = void (privateRunQuery_ q params session)

execute' :: forall eff. Query Unit -> Session -> Aff (db :: DB | eff) Unit
execute' q = execute q (mkParams {})

query :: forall eff a. (IsForeign a) => Query a -> Params -> Session -> Aff (db :: DB | eff) (Array a)
query q params session =
  either liftError pure =<< map (traverse read) (privateRunQuery_ q params session)

query' :: forall eff a. (IsForeign a) => Query a -> Session -> Aff (db :: DB | eff) (Array a)
query' q = query q (mkParams {})

privateRunQuery_ :: forall eff a. Query a -> Params -> Session -> Aff (db :: DB | eff) (Array Foreign)
privateRunQuery_ q (Params params) session =
  makeAff (\reject accept -> runFn6 runQuery_ error reject accept session (show q) params)

closeSession :: forall eff. Session -> Aff (db :: DB | eff) Unit
closeSession session = makeAff (\reject accept -> runFn2 closeSession_ accept session)

closeDriver :: forall eff. Driver -> Aff (db :: DB | eff) Unit
closeDriver driver = makeAff (\reject accept -> runFn2 closeDriver_ accept driver)

liftError :: forall e a. ForeignError -> Aff e a
liftError err = throwError $ error (show err)

foreign import mkAuth_ :: Username -> Password -> Foreign

foreign import mkDriver_ :: forall eff. Fn3 String Foreign Foreign (Eff (db :: DB | eff) Driver)

foreign import mkSession_ :: forall eff. Fn1 Driver (Eff (db :: DB | eff) Session)

foreign import runQuery_ :: forall eff. Fn6 (String -> Error) (Error -> Eff (db :: DB | eff) Unit) (Array Foreign -> Eff (db :: DB | eff) Unit) Session String Foreign (Eff (db :: DB | eff) Unit)

foreign import closeSession_ :: forall eff. Fn2 (Unit -> Eff (db :: DB | eff) Unit) Session (Eff (db :: DB | eff) Unit)

foreign import closeDriver_ :: forall eff. Fn2 (Unit -> Eff (db :: DB | eff) Unit) Driver (Eff (db :: DB | eff) Unit)
