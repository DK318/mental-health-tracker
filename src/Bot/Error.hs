module Bot.Error where

data BotError
  = NoSuchEmotion
  | EmotionSubtypeParseError
  | NoUserEmotions
  deriving stock (Show)
