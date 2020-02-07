module Backend.RequestHandler where

import Database.Beam
-- import qualified Database.Beam.Postgres as Pg
import qualified Database.Beam.Backend.SQL.BeamExtensions as Ext
import Rhyolite.Api (ApiRequest (..))
import Rhyolite.Backend.App (RequestHandler (..))

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (PrivateRequest (..), PublicRequest (..))
import Common.Schema

--import Common.Prelude

requestHandler :: (forall x. Transaction mode x -> m x) -> RequestHandler (ApiRequest () PublicRequest PrivateRequest) m
requestHandler runTransaction =
  RequestHandler $ runTransaction . \case
    ApiRequest_Public r -> case r of
      PublicRequest_NoOp -> pure ()
      PublicRequest_CreatePerson name tz -> do
        pId <- runQuery $ do
          [personT] <- Ext.runInsertReturningList $ insert (_dbPerson db) $ (insertExpressions
            [ Person
              { _personId = default_
              , _personName = val_ name
              , _personTimezone = val_ tz
              , _personAfkNote = val_ Nothing
              }
            ])
          pure  (PersonId $ _personId personT)

        notify Notification_Person (Added, pId)
        pure pId

      PublicRequest_MarkPersonAfk pId note -> do
        runQuery $ runUpdate $ update (_dbPerson db)
          (\p -> _personAfkNote p <-. val_ (Just note))
          (\p -> _personId p ==. val_ (unPersonId pId))
        notify Notification_Person (Modified, pId)

      PublicRequest_MarkPersonBack pId -> do
        runQuery $ runUpdate $ update (_dbPerson db)
          (\p -> _personAfkNote p <-. val_ Nothing)
          (\p -> _personId p ==. val_ (unPersonId pId))
        notify Notification_Person (Modified, pId)

    ApiRequest_Private _key r -> case r of
      PrivateRequest_NoOp -> return ()
