module Database.Neo4J.Types where

import Prelude
import Data.Foreign (Foreign, toForeign, ForeignError)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Generic (readGeneric, toForeignGeneric, Options, defaultOptions)
import Data.Generic (class Generic, gShow)

-- | A `Query` returns an array of `a` records.
newtype Query a = Query String

instance eqQuery :: Eq (Query a) where
  eq (Query a) (Query b) = a == b
instance showQuery :: Show (Query a) where
  show (Query n) = n

-- | A convenience type for queries with one item of interest, which must be
-- | denoted by an `x`. For example:
-- |
-- | `Query "MATCH (x:Person) RETURN x" :: Query' (Node Person)` -- returns {x: Node { ..., properties: Person {...}}}
-- | `Query "MATCH (a:Person) RETURN a.name as x" :: Query' String` -- returns {x: "..."}
type Query' a = Query (XRecord a)

-- | Query parameters.
newtype Params = Params Foreign

defaultForeignOptions :: Options
defaultForeignOptions = defaultOptions { unwrapNewtypes = true }

-- | Turns a record such as `{name: "Arthur"}` into parameters for a query.
mkParams :: forall a. a -> Params
mkParams = Params <<< toForeign

newtype NeoInteger = NeoInteger
  { high :: Int
  , low :: Int
  }

derive instance genericNeoInt :: Generic NeoInteger
derive instance eqNeoInt :: Eq NeoInteger

instance showNeoInt :: Show NeoInteger where
  show = gShow

instance isForeignNeoInt :: IsForeign NeoInteger where
  read = readGeneric defaultForeignOptions

newtype Node a = Node
  { identity :: NeoInteger
  , labels :: Array String
  , properties :: a
  }

derive instance genericNode :: (Generic a) => Generic (Node a)
derive instance eqNode :: (Eq a) => Eq (Node a)

instance showNode :: (Generic a) => Show (Node a) where
  show = gShow

instance isForeignNode :: (Generic (Node a)) => IsForeign (Node a) where
  read = readGeneric defaultForeignOptions

getProperties :: forall a. Node a -> a
getProperties (Node {identity, labels, properties}) = properties

newtype Relationship a = Relationship
  { identity :: NeoInteger
  , start :: NeoInteger
  , end :: NeoInteger
  , "type" :: String
  , properties :: a
  }

derive instance genericRelationship :: (Generic a) => Generic (Relationship a)
derive instance eqRelationship :: (Eq a) => Eq (Relationship a)

instance showRelationship :: (Generic a) => Show (Relationship a) where
  show = gShow

instance isForeignRelationship :: (Generic (Relationship a)) => IsForeign (Relationship a) where
  read = readGeneric defaultForeignOptions

-- | A type for relationships with no added properties
type Relationship' = Relationship NoProps

newtype XRecord a = XRecord
  { x :: a }

derive instance genericXRecord :: (Generic a) => Generic (XRecord a)
derive instance eqXRecord :: (Eq a) => Eq (XRecord a)

instance showXRecord :: (Generic a) => Show (XRecord a) where
  show = gShow

instance isForeignXRecord :: (Generic (XRecord a)) => IsForeign (XRecord a) where
  read = readGeneric defaultForeignOptions

unbox :: forall a. XRecord a -> a
unbox (XRecord {x}) = x

newtype NoProps = NoProps
  { }

derive instance genericNoProps :: Generic NoProps
derive instance eqNoProps :: Eq NoProps
instance showNoProps :: Show NoProps where
  show = const ""
instance isForeignNoProps :: IsForeign NoProps where
  read = readGeneric defaultForeignOptions

