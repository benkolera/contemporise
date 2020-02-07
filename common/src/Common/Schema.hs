{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Schema where

import qualified Data.Aeson as Json
import Data.Text (Text)
import Database.Beam (Beamable, Nullable, Columnar, PrimaryKey, Table (primaryKey))
import Database.Beam.Backend.SQL.Types (SqlSerial)
import GHC.Generics (Generic)

import Common.Prelude

-------------------------------------------------------------------------------
data PersonT f = Person
  { _personId :: Columnar f (SqlSerial Int)
  , _personName :: Columnar f Text
  , _personTimezone :: Columnar f Text
  , _personAfkNote :: Columnar f (Maybe Text)
  } deriving (Generic, Beamable)

instance Table PersonT where
  data PrimaryKey PersonT f = PersonId { unPersonId :: Columnar f (SqlSerial Int) }
    deriving stock Generic
    deriving anyclass Beamable
  primaryKey = PersonId . _personId

type Person = PersonT Identity
deriving instance Eq Person
deriving instance Show Person
instance FromJSON Person
instance ToJSON Person

type PersonId = PrimaryKey PersonT Identity
deriving instance Eq PersonId
deriving instance Ord PersonId
deriving instance Show PersonId
instance FromJSON PersonId
instance ToJSON PersonId
instance Json.ToJSONKey PersonId
instance Json.FromJSONKey PersonId
-------------------------------------------------------------------------------
