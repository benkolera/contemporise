{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-} -- For deriveJSONGADT
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common.App where

import Control.Lens.TH (makeLenses)
import Control.Monad (mfilter)
import Data.Aeson (parseJSON, toJSON)
import qualified Data.Aeson as Json
import Data.Align (Align (nil), Semialign (alignWith))
import qualified Data.Align as Align
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Semigroup (option)
import Data.Witherable (Witherable (wither))
import qualified Data.Map.Merge.Lazy as Map
import qualified Data.Map.Monoidal as MMap
import qualified Data.Set as Set
import Data.MonoidMap (MonoidMap)
import Data.Semigroup (First (..))
import Reflex.Query.Class (Query (QueryResult, crop), SelectedCount (..))
import Reflex.Patch (Additive, Group (negateG))
import Rhyolite.App (PositivePart (positivePart), standardPositivePart)

import Common.Prelude
import Common.Schema

data PublicRequest a where
  PublicRequest_NoOp :: PublicRequest ()
  PublicRequest_CreatePerson :: Text -> Text -> PublicRequest PersonId
  PublicRequest_MarkPersonAfk :: PersonId -> Text -> PublicRequest ()
  PublicRequest_MarkPersonBack :: PersonId -> PublicRequest ()
deriving instance Show a => Show (PublicRequest a)

fmap concat $ sequence
  [ deriveJSONGADT ''PublicRequest
  , deriveArgDict ''PublicRequest
  ]

data PrivateRequest a where
  PrivateRequest_NoOp :: PrivateRequest ()
fmap concat $ sequence
  [ deriveJSONGADT ''PrivateRequest
  , deriveArgDict ''PrivateRequest
  ]
deriving instance Show a => Show (PrivateRequest a)

-- ORPHANS
-- https://github.com/isomorphism/these/pull/121
deriving newtype instance Semialign Option
deriving newtype instance Align Option

-- https://github.com/fumieval/witherable/pull/43
instance Filterable Option where
  mapMaybe f = (>>= Option . f)
  {-# INLINE mapMaybe #-}
instance Witherable Option where
  wither f (Option x) = Option <$> wither f x
  {-# INLINE wither #-}

data ViewSelector a = ViewSelector
  { _viewSelector_allPeople :: !(Option a)
  , _viewSelector_people :: !(MonoidalMap PersonId a)
  }
  deriving (Eq, Functor, Generic)
deriveJSON Json.defaultOptions 'ViewSelector
makeLenses 'ViewSelector
instance Semigroup a => Semigroup (ViewSelector a) where
  a <> b = ViewSelector
    { _viewSelector_allPeople = _viewSelector_allPeople a <> _viewSelector_allPeople b
    , _viewSelector_people = _viewSelector_people a <> _viewSelector_people b
    }
instance Semigroup a => Monoid (ViewSelector a) where
  mempty = ViewSelector mempty mempty
  mappend = (<>)
instance Semialign ViewSelector where
  alignWith f a b = ViewSelector
    { _viewSelector_allPeople = alignWith f (_viewSelector_allPeople a) (_viewSelector_allPeople b)
    , _viewSelector_people = alignWith f (_viewSelector_people a) (_viewSelector_people b)
    }
  zipWith f a b = ViewSelector
    { _viewSelector_allPeople = Align.zipWith f (_viewSelector_allPeople a) (_viewSelector_allPeople b)
    , _viewSelector_people = Align.zipWith f (_viewSelector_people a) (_viewSelector_people b)
    }
instance Align ViewSelector where
  nil = ViewSelector nil nil
instance (Group a) => Group (ViewSelector a) where
  negateG = fmap negateG
instance (Semigroup a) => Additive (ViewSelector a)
instance (Ord k) => PositivePart (ViewSelector (MonoidMap k SelectedCount)) where
  positivePart x =
    let u = mapMaybe standardPositivePart x
    in if u == mempty then Nothing else Just u
instance Filterable ViewSelector where
  mapMaybe f x = ViewSelector
    { _viewSelector_allPeople = mapMaybe f (_viewSelector_allPeople x)
    , _viewSelector_people = mapMaybe f (_viewSelector_people x)
    }
instance (Eq a, Monoid a) => Query (ViewSelector a) where
  type QueryResult (ViewSelector a) = View a
  crop vs v = View
    { _view_allPeople = mfilter (const $ option False (const True) $ _viewSelector_allPeople vs) (_view_allPeople v)
    , _view_people = croppedIntersectionWith (flip const) (_viewSelector_people vs) (_view_people v)
    }
-- Intersect a map from the viewselector and a map from the view to produce a cropped map for the view, dropping any key for which the entry is selected mempty (zero) times.
croppedIntersectionWith :: (Ord k, Eq a, Monoid a) => (a -> b -> c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
croppedIntersectionWith f (MMap.MonoidalMap m) (MMap.MonoidalMap m') = MMap.MonoidalMap $
  Map.merge
    Map.dropMissing
    Map.dropMissing
    (Map.zipWithMaybeMatched (\_ a v -> if a == mempty then Nothing else Just (f a v)))
    m
    m'

data View a = View
  { _view_allPeople :: !(Option (a, Set.Set PersonId))
  , _view_people :: !(MonoidalMap PersonId (a, First Person))
  }
  deriving (Eq, Foldable, Functor, Generic)
deriveJSON Json.defaultOptions 'View
makeLenses 'View
instance Monoid a => Semigroup (View a) where
  a <> b = View
    { _view_allPeople = _view_allPeople a <> _view_allPeople b
    , _view_people = _view_people a <> _view_people b
    }
instance Monoid a => Monoid (View a) where
  mempty = View mempty mempty
  mappend = (<>)
instance Filterable View where
  mapMaybe f x = View
    { _view_allPeople = mapMaybeView f (_view_allPeople x)
    , _view_people = mapMaybeView f (_view_people x)
    }
    
mapMaybeView
  :: forall f v a b. (Filterable f)
  => (a -> Maybe b)
  -> f (a, v)
  -> f (b, v)
mapMaybeView f = mapMaybe ((_1 :: (a -> Maybe b) -> (a, v) -> Maybe (b, v)) f)
