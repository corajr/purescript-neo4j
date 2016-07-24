module Test.Fixture where

import Prelude
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric)
import Data.Generic (class Generic, gShow, gEq)
import Database.Neo4J (defaultForeignOptions, Node, NeoInteger)

newtype Person = Person
  { name :: String
  , age :: NeoInteger
  }

derive instance genericPerson :: Generic Person
derive instance eqPerson :: Eq Person
instance showPerson :: Show Person where
  show = gShow
instance isForeignPerson :: IsForeign Person where
  read = readGeneric defaultForeignOptions


-- To retrieve query results, one must create a newtype for the result record
-- containing all the desired fields from the query. The `XRecord` type is
-- provided for queries with just one output, but if more flexibility is needed,
-- types can be defined like so:

newtype NameRec = NameRec
  { "a.name" :: String }

derive instance genericNameRec :: Generic NameRec
derive instance eqNameRec :: Eq NameRec
instance showNameRec :: Show NameRec where
  show = gShow
instance isForeignNameRec :: IsForeign NameRec where
  read = readGeneric defaultForeignOptions
