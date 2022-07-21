module Emanima.Model where

import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Emanote.Model.Type qualified as Em

data Model = Model
  { modelStatic :: SR.Model
  , modelNotes :: Em.ModelEma
  }
  deriving stock (Generic)
