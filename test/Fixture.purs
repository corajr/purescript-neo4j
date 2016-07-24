module Test.Fixture where

import Prelude
import Data.Foreign.Class (class IsForeign)
import Data.Foreign.Generic (readGeneric)
import Data.Generic (class Generic, gShow, gEq)
import Database.Neo4J (defaultForeignOptions, Node, Relationship, NeoInteger)

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

newtype NameRec = NameRec
  { "a.name" :: String }

derive instance genericNameRec :: Generic NameRec
derive instance eqNameRec :: Eq NameRec
instance showNameRec :: Show NameRec where
  show = gShow
instance isForeignNameRec :: IsForeign NameRec where
  read = readGeneric defaultForeignOptions

newtype AgeRec = AgeRec
  { "a.age" :: NeoInteger }

derive instance genericAgeRec :: Generic AgeRec
derive instance eqAgeRec :: Eq AgeRec
instance showAgeRec :: Show AgeRec where
  show = gShow
instance isForeignAgeRec :: IsForeign AgeRec where
  read = readGeneric defaultForeignOptions

newtype NodePersonRec = NodePersonRec
  { a :: Node Person }

derive instance genericNodePersonRec :: Generic NodePersonRec
derive instance eqNodePersonRec :: Eq NodePersonRec
instance showNodePersonRec :: Show NodePersonRec where
  show = gShow
instance isForeignNodePersonRec :: IsForeign NodePersonRec where
  read = readGeneric defaultForeignOptions

newtype Friend = Friend
  { }

derive instance genericFriend :: Generic Friend
derive instance eqFriend :: Eq Friend
instance showFriend :: Show Friend where
  show = gShow
instance isForeignFriend :: IsForeign Friend where
  read = readGeneric defaultForeignOptions

newtype FriendRec = FriendRec
  { r :: Relationship Friend }

derive instance genericFriendRec :: Generic FriendRec
derive instance eqFriendRec :: Eq FriendRec
instance showFriendRec :: Show FriendRec where
  show = gShow
instance isForeignFriendRec :: IsForeign FriendRec where
  read = readGeneric defaultForeignOptions



