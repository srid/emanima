{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Emanima.Site where

import Data.Generics.Sum.Any (AsAny (_As))
import Ema
import Emanima.Model
import Emanima.Route
import Emanima.View qualified as View
import Emanote ()
import Emanote qualified as Em
import Emanote.CLI qualified as Em
import Emanote.Route.SiteRoute.Type qualified as Em
import Optics.Core ((%))

instance EmaSite Route where
  type SiteArg Route = SiteArg Em.SiteRoute
  siteInput cliAct emanoteConfig = do
    staticRouteDyn <- siteInput @StaticRoute cliAct ()
    notesDyn <- siteInput @Em.SiteRoute cliAct emanoteConfig
    pure $ Model <$> staticRouteDyn <*> notesDyn
  siteOutput rp m = \case
    Route_Html hr ->
      case hr of
        HtmlRoute_Index ->
          Ema.AssetGenerated Ema.Html $ View.renderDashboard rp m
        HtmlRoute_Notes r ->
          siteOutput (rp % (_As @"Route_Html") % (_As @"HtmlRoute_Notes")) (modelNotes m) r
    Route_Static r ->
      siteOutput (rp % (_As @"Route_Static")) (modelStatic m) r

runEmanima :: IO ()
runEmanima = do
  -- TODO: Use cliParser to have control over CLI banner.
  (emCfg, cli) <- (Em.defaultEmanoteConfig &&& Em.emaCli) <$> Em.parseCli
  void $ Ema.runSiteWithCli @Route cli emCfg
