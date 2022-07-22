module Emanima.Model where

import Data.Map.Strict qualified as Map
import Data.Time.Calendar (Day)
import Ema.Route.Lib.Extra.StaticRoute qualified as SR
import Emanote.Model.Calendar qualified as Em
import Emanote.Model.Note qualified as Em
import Emanote.Model.Type qualified as Em
import Emanote.Route qualified as Em

data Model = Model
  { modelStatic :: SR.Model
  , modelNotes :: Em.ModelEma
  , modelMoodDeltas :: Map Day (Em.LMLRoute, MoodDelta)
  }
  deriving stock (Generic)

data MoodDelta = Up | Same | Down
  deriving stock (Eq, Show, Ord, Read, Generic)

parseMoodDeltas :: [Em.Note] -> Map Day (Em.LMLRoute, MoodDelta)
parseMoodDeltas notes =
  Map.fromList . catMaybes . flip fmap notes $ \note -> do
    (day, delta) <- parseMoodDelta note
    pure (day, (Em._noteRoute note, delta))

parseMoodDelta :: Em.Note -> Maybe (Day, MoodDelta)
parseMoodDelta note = do
  day <- Em.parseRouteDay $ Em._noteRoute note
  let fixCase :: String -> String = \case
        "up" -> "Up"
        "down" -> "Down"
        "same" -> "Same"
        x -> x
  moodDelta :: MoodDelta <- readMaybe . traceShowId . fixCase =<< Em.lookupMeta (one "mood") note
  pure (day, moodDelta)