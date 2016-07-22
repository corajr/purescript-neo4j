module Database.Neo4J where

import Prelude
import Data.Function.Uncurried
import Control.Monad.Aff (Aff, makeAff, finally, liftEff')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans   (ReaderT(), runReaderT, ask)
import Control.Monad.Trans (lift)
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

type InTransaction eff a = ReaderT Transaction (Aff (db :: DB | eff)) a

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
  { encrypted: false
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

withDriver :: forall eff a.
              ConnectionInfo
           -> (Driver -> Aff (db :: DB | eff) a)
           -> Aff (db :: DB | eff) a
withDriver info f = do
  driver <- liftEff $ mkDriver info
  finally (f driver) $ closeDriver driver

withSession :: forall eff a.
               Driver
            -> (Session -> Aff (db :: DB | eff) a)
            -> Aff (db :: DB | eff) a
withSession driver f = do
  session <- liftEff $ mkSession driver
  finally (f session) $ closeSession session

withTransaction :: forall eff a.
                   Session
                -> InTransaction eff a
                -> Aff (db :: DB | eff) a
withTransaction session f = do
  transaction <- liftEff $ runFn1 beginTransaction_ session
  finally (runReaderT f transaction) (liftEff $ runFn1 commitTransaction_ transaction)

withRollback :: forall eff a.
                Session
             -> InTransaction eff a
             -> Aff (db :: DB | eff) a
withRollback session f = do
  transaction <- liftEff $ runFn1 beginTransaction_ session
  finally (runReaderT f transaction) (liftEff $ runFn1 rollbackTransaction_ transaction)

execute :: forall eff. Query Unit -> Params -> InTransaction eff Unit
execute q params = void (privateRunQuery_ q params)

execute' :: forall eff. Query Unit -> InTransaction eff Unit
execute' q = execute q (mkParams {})

query :: forall eff a. (IsForeign a) => Query a -> Params -> InTransaction eff (Array a)
query q params = do
  results <- privateRunQuery_ q params
  lift $ either liftError pure (traverse read results)

query' :: forall eff a. (IsForeign a) => Query a -> InTransaction eff (Array a)
query' q = query q (mkParams {})

privateRunQuery_ :: forall eff a. Query a -> Params -> InTransaction eff (Array Foreign)
privateRunQuery_ q (Params params) = do
  transaction <- ask
  lift $ makeAff (\reject accept -> runFn6 runQuery_ error reject accept transaction (show q) params)

closeSession :: forall eff. Session -> Aff (db :: DB | eff) Unit
closeSession session = liftEff $ runFn1 closeSession_ session

closeDriver :: forall eff. Driver -> Aff (db :: DB | eff) Unit
closeDriver driver = liftEff $ runFn1 closeDriver_ driver

liftError :: forall e a. ForeignError -> Aff e a
liftError err = throwError $ error (show err)

foreign import mkAuth_ :: Username -> Password -> Foreign

foreign import mkDriver_ :: forall eff. Fn3 String Foreign Foreign (Eff (db :: DB | eff) Driver)

foreign import mkSession_ :: forall eff. Fn1 Driver (Eff (db :: DB | eff) Session)

foreign import runQuery_ :: forall eff. Fn6 (String -> Error) (Error -> Eff (db :: DB | eff) Unit) (Array Foreign -> Eff (db :: DB | eff) Unit) Transaction String Foreign (Eff (db :: DB | eff) Unit)

foreign import beginTransaction_ :: forall eff. Fn1 Session (Eff (db :: DB | eff) Transaction)

foreign import commitTransaction_ :: forall eff. Fn1 Transaction (Eff (db :: DB | eff) Unit)

foreign import rollbackTransaction_ :: forall eff. Fn1 Transaction (Eff (db :: DB | eff) Unit)

foreign import closeSession_ :: forall eff. Fn1 Session (Eff (db :: DB | eff) Unit)

foreign import closeDriver_ :: forall eff. Fn1 Driver (Eff (db :: DB | eff) Unit)
