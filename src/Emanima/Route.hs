{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Emanima.Route where

import Ema.Route.Generic.TH
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Emanima.Model (Model)
import Emanote ()
import Emanote.Model.Type qualified as Em
import Emanote.Route.SiteRoute.Type qualified as Em

data HtmlRoute
  = HtmlRoute_Index
  | HtmlRoute_Notes Em.SiteRoute
  deriving stock (Show, Eq, Ord, Generic)

deriveGeneric ''HtmlRoute
deriveIsRoute ''HtmlRoute [t|'[WithModel Em.ModelEma]|]

type StaticRoute = SR.StaticRoute "static"

data Route
  = Route_Html HtmlRoute
  | Route_Static StaticRoute
  deriving stock (Eq, Show, Ord, Generic)

deriveGeneric ''Route
deriveIsRoute
  ''Route
  [t|
    [ -- To render a `Route` we need `Model`
      WithModel Model
    , -- Override default sub-route encoding (to avoid the prefix in encoded URLs)
      WithSubRoutes [HtmlRoute, StaticRoute]
    ]
    |]
