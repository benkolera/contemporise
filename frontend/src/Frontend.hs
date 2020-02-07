{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Lens

--import Control.Monad (join)
import Control.Category ((>>>))
import Control.Monad.Fix (MonadFix)
import Data.Foldable (foldMap)
import qualified Data.Map.Monoidal as MMap
import qualified Data.Text as T
import Data.Semigroup (getFirst)
import qualified Data.Set as Set
import Obelisk.Configs (HasConfigs)
import Obelisk.Frontend (Frontend (..))
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Api (ApiRequest(ApiRequest_Public))
import Rhyolite.Frontend.App (RhyoliteWidget, functorToWire, runObeliskRhyoliteWidget, watchViewSelector)

import Obelisk.Generated.Static

import Common.App
import Common.Route
import Common.Prelude
import Common.Schema


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = headSection
  , _frontend_body = runAppWidget $ skeleton $ subRoute_ $ \case
      FrontendRoute_Main -> appWidget
  }

headSection :: DomBuilder t m => m ()
headSection = do
  elAttr "meta" ("charset"=:"utf-8") blank
  elAttr "meta" ("name"=:"viewport" <> "content"=:"width=device-width, initial-scale=1") blank
  elAttr "link" ("rel"=:"stylesheet" <> "type"=:"text/css" <> "href"=: static @"css/bulma.css") blank
  el "title" $ text "Cogitate"
  elAttr "script" ("defer"=:"defer"<> "src"=:"https://use.fontawesome.com/releases/v5.3.1/js/all.js") blank

runAppWidget ::
  ( HasConfigs m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadHold t m
  , PostBuild t m
  , MonadFix m
  , Prerender x t m
  )
  => RoutedT t (R FrontendRoute) (RhyoliteWidget (ViewSelector SelectedCount) (ApiRequest () PublicRequest PrivateRequest) t m) a
  -> RoutedT t (R FrontendRoute) m a
runAppWidget = runObeliskRhyoliteWidget
  functorToWire
  "common/route"
  checkedFullRouteEncoder
  (BackendRoute_Listen :/ ())


skeleton
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, SetRoute t (R FrontendRoute) m)
  => m a -> m a
skeleton body = do
  navBar
  a <- elClass "section" "section" body
  footer
  pure a


navBar
  :: (DomBuilder t m, MonadHold t m, PostBuild t m, MonadFix m, SetRoute t (R FrontendRoute) m)
  => m ()
navBar =
  elAttr "nav" ("class"=:"navbar" <> "role"=:"navigation" <> "aria-label"=:"main navigation") $ do
    divClass "navbar-brand" $
      elAttr "a" ("class"=:"navbar-item" <> "href"=:"https://bulma.io") $ text "Cogitate"

    (burgerEl, ()) <- elAttr' "a" ("role"=:"button" <> "class"=:"navbar-burger burger" <> "aria-label"=:"menu") $ do
      let ln = elAttr "span" ("aria-hidden"=:"true") blank
      ln *> ln *> ln

    menuIsActive <- toggle False $ domEvent Click burgerEl

    elDynAttr "div" (ffor menuIsActive $ \active -> "class"=:("navbar-menu" <> if active then " is-active" else "")) $ do
      divClass "navbar-start" $ do
        (homeEl, _) <- elClass' "a" "navbar-item" $ text "Home"
        setRoute $ FrontendRoute_Main :/ () <$ domEvent Click homeEl

footer :: DomBuilder t m => m ()
footer =
  elClass "footer" "footer" $
    divClass "content has-text-centered" $
      el "p" $ do
        el "strong" (text "Cogitate") *> text " by Ben Kolera. The source code is licensed "
        elAttr "a" ("href"=:"http://opensource.org/licenses/mit-license.php") (text "MIT")

type HasApp t m =
  ( MonadQuery t (ViewSelector SelectedCount) m
  , Requester t m
  , Request m ~ ApiRequest () PublicRequest PrivateRequest
  , Response m ~ Identity
  )

appWidget
  :: forall m t js. (Prerender js t m, HasApp t m, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m ()
appWidget = do
  el "h1" $ text "People"
  eClick <- button "Add Ben"
  requesting_ $ ApiRequest_Public (PublicRequest_CreatePerson "Ben" "Australia/Brisbane")  <$ eClick
  dPeopleIdsMay <- watchAllPeople
  dPeople <- watchPeople $ fromMaybe mempty <$> dPeopleIdsMay
  el "ul" $ listWithKey (MMap.getMonoidalMap <$> dPeople) $ \pId dPerson -> do
    el "li" $ do
      dynText $ _personName <$> dPerson
      text ":"
      dynText $ _personTimezone <$> dPerson
      dyn_ $ ffor dPerson $ _personAfkNote >>> \case
        Nothing -> do
          eAfk <- button "AFK"
          requesting_ $ ApiRequest_Public (PublicRequest_MarkPersonAfk pId "") <$ eAfk
        Just n -> do
          text $ "(AFK" <> (if T.null n then "" else ": " <> n) <> ")"
          text " "
          eAfk <- button "Back"
          requesting_ $ ApiRequest_Public (PublicRequest_MarkPersonBack pId) <$ eAfk
      
  pure ()

watchAllPeople
  :: (HasApp t m, MonadHold t m, MonadFix m)
  =>  m (Dynamic t (Maybe (Set.Set PersonId)))
watchAllPeople = do
  res :: Dynamic t (View SelectedCount) <- watchViewSelector $ constDyn $ (mempty :: ViewSelector SelectedCount)
    { _viewSelector_allPeople = pure 1 }
  pure $ ffor res $ \r -> r ^? view_allPeople . to getOption . _Just . _2

watchPeople
  :: (HasApp t m, MonadHold t m, MonadFix m)
  => Dynamic t (Set PersonId)
  -> m (Dynamic t (MMap.MonoidalMap PersonId Person))
watchPeople dPeopleIds = do
  res :: Dynamic t (View SelectedCount) <- watchViewSelector $ ffor dPeopleIds $ \peopleIds -> (mempty :: ViewSelector SelectedCount)
    { _viewSelector_people = MMap.fromList . fmap (,1) . Set.toList $ peopleIds }
  pure . ffor res $ \r -> fmap (^. _2 . to getFirst) (r ^. view_people)

watchPerson
  :: (HasApp t m, MonadHold t m, MonadFix m)
  => Dynamic t PersonId
  -> m (Dynamic t (Maybe Person))
watchPerson dPersonId = do
  res :: Dynamic t (View SelectedCount) <- watchViewSelector $ ffor dPersonId $ \personId -> (mempty :: ViewSelector SelectedCount)
    { _viewSelector_people = MMap.singleton personId 1 }
  pure . ffor2 dPersonId res $ \personId r -> r ^? view_people . ix personId . _2 . to getFirst
