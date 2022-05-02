module Bot.State where

import Control.Lens
import Telegram.API (Token)
import Data.Map (Map)
import qualified Data.Map as M
import Bot.Emotions (UserEmotion, SomeEmotion, TrackedEmotion)

data BotState = BotState
  { beToken :: Token
  , beOffset :: Integer
  , beDatabase :: Map Integer [UserEmotion]
  , beAsked :: Map Integer Bool
  , beSomeEmotions :: Map Integer SomeEmotion
  , beTracked :: Map Integer TrackedEmotion
  }
makeLensesWith abbreviatedFields ''BotState

initBotState :: Token -> BotState
initBotState token = BotState token 0 M.empty M.empty M.empty M.empty
