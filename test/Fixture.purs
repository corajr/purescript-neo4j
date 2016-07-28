module Test.Fixture where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Generic (readGeneric)
import Data.Foreign.NullOrUndefined (unNullOrUndefined, NullOrUndefined(NullOrUndefined))
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

--  Also note that only an IsForeign instance is needed, not Generic. (Show and
--  Eq needed for test purposes.)
newtype AgeRec = AgeRec
  { "a.age" :: NeoInteger }
derive instance eqAgeRec :: Eq AgeRec
instance showAgeRec :: Show AgeRec where
  show (AgeRec { "a.age": age}) = "{ 'a.age': " <> show age <> " }"
instance isForeignAgeRec :: IsForeign AgeRec where
  read value = do
    x <- readProp "a.age" value
    pure $ AgeRec {"a.age": x}

-- Can use I
newtype OptionalFields = OptionalFields
  { name :: String
  , age :: NullOrUndefined NeoInteger }
instance isForeignOptionalFields :: IsForeign OptionalFields where
  read value = do
    name <- readProp "name" value
    age <- readProp "age" value
    pure $ OptionalFields { name: name, age: age}

newtype EqOptionalFields = EqOptionalFields
  { name :: String
  , age :: Maybe NeoInteger }
derive instance eqEqOptional :: Eq EqOptionalFields
instance showEqOptional :: Show EqOptionalFields where
  show (EqOptionalFields {name, age}) = "EqOptionalFields { name: " <> show name <> ", age: " <> show age <> " }"

unwrapOptionalFields :: OptionalFields -> EqOptionalFields
unwrapOptionalFields (OptionalFields { name, age}) =
  EqOptionalFields { name: name, age: unNullOrUndefined age }
