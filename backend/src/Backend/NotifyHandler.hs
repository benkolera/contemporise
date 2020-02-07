module Backend.NotifyHandler where

import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Data.Semigroup (First(..))
import Rhyolite.Backend.Listen (DbNotification (..))
import Data.Dependent.Sum      (DSum ((:=>)))

import Backend.Transaction (Transaction)
import Backend.Schema (Notification (..), Change(..))
import Backend.Schema.Person (getPerson)
import Common.App (View (..), ViewSelector (..))
import Common.Prelude

notifyHandler :: forall a. Monoid a => (forall x. (forall mode. Transaction mode x) -> IO x) -> DbNotification Notification -> ViewSelector a -> IO (View a)
notifyHandler runTransaction msg vs = case _dbNotification_message msg of
  Notification_Person :=> Identity (change, pId) -> do
    case change of
      Added -> pure $ mempty
        { _view_allPeople = (,Set.singleton pId) <$> _viewSelector_allPeople vs }
      Modified -> do
        let mWatched = MMap.lookup pId (_viewSelector_people vs)
        mPerson <- runTransaction $ getPerson pId
        pure $ mempty
          { _view_people = fold $ (\a p -> MMap.singleton pId (a,First p)) <$> mWatched <*> mPerson }
