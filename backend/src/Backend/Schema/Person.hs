module Backend.Schema.Person where

import Database.Beam
import qualified Data.Set as Set

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.Prelude
import Common.Schema

getPerson :: PersonId -> Transaction mode (Maybe Person)
getPerson personId = fmap headMay $
  runQuery $ runSelectReturningList $ select $ do
    person <- all_ (_dbPerson db)
    guard_ (_personId person ==. val_ (unPersonId personId))
    pure person

getPeopleIds :: Transaction mode (Set PersonId)
getPeopleIds = fmap (Set.fromList . fmap PersonId) $ runQuery $ runSelectReturningList $ select $
  _personId <$> all_ (_dbPerson db)
