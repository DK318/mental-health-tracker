module Telegram.JsonExt where

import Data.Aeson

dropOptions :: Int -> Options
dropOptions n = defaultOptions
  { fieldLabelModifier = drop n
  , omitNothingFields = True
  }
