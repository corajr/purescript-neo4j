module Test.Fixture where

import Prelude
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric)
import Data.Generic (class Generic, gShow, gEq)
import Database.Neo4J (defaultForeignOptions)

newtype Person = Person
  { name :: String
  , title :: String
  }

derive instance genericPerson :: Generic Person

instance showPerson :: Show Person where
  show = gShow

instance eqPerson :: Eq Person where
  eq = gEq

instance isForeignPerson :: IsForeign Person where
  read = readGeneric defaultForeignOptions
