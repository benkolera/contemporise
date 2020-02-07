{-# LANGUAGE FlexibleContexts #-}
module Backend.ViewSelectorHandler where

import Data.Semigroup (First(First))
import qualified Data.Set as Set
import Database.Beam

import Backend.Schema
import Backend.Transaction (Transaction, runQuery)
import Common.App (View (..), ViewSelector (..))
import Common.Schema
import Common.Prelude
import Backend.Schema.Person

viewSelectorHandler :: (Eq a, Monoid a) => (forall x. (forall mode. Transaction mode x) -> IO x) -> ViewSelector a -> IO (View a)
viewSelectorHandler runTransaction vs = if vs == mempty then pure mempty else runTransaction $ do
  peopleIds <- for (_viewSelector_allPeople vs) $ \a ->
    (a,) <$> getPeopleIds
  people <- ifor (_viewSelector_people vs) $ \personId a ->
    (a,) <$> getPerson personId
  pure $ View
    { _view_allPeople = peopleIds
    , _view_people = mapMaybe (\(a,me) -> (a,) . First <$> me) people
    }
