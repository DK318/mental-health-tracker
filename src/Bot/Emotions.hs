{-# LANGUAGE OverloadedLists #-}
module Bot.Emotions where

import Control.Lens hiding ((.=))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Time (UTCTime)
import Data.Csv (ToNamedRecord (toNamedRecord), namedRecord, (.=), Header, header)
import Data.Maybe (fromMaybe)

data RageSubtype
  = Rabies
  | Fury
  | Anger
  | Hatred
  | Disgust
  | Disturbance
  | Indignation
  | Wrath
  | Irritation
  | Contempt
  | Dislike
  | Vulnerability
  | Annoyance
  deriving stock (Show, Eq, Ord)

data FearSubtype
  = Horror
  | Fright
  | Fear
  | Anxiety
  | Worry
  | Apprehension
  | Perplexity
  | Timidity
  deriving stock (Show, Eq, Ord)

data JoySubtype
  = Delight
  | Admiration
  | Rapture
  | Exultation
  | Happiness
  | Joy
  | Elation
  | Appeasement
  | Tenderness
  | Pride
  | Gratitude
  | Inspiration
  | Pleasure
  deriving stock (Show, Eq, Ord)

data SadnessSubtype
  = Sorrow
  | Grief
  | Despair
  | Yearning
  | Resentment
  | Emptiness
  | Melancholy
  | Sadness
  | Despondency
  | Helplessness
  | Regret
  | Weakness
  | Depression
  deriving stock (Show, Eq, Ord)

data SurpriseSubtype
  = Stun
  | Amazement
  | Astonishment
  | Confusion
  deriving stock (Show, Eq, Ord)

data ShameSubtype
  = Cringe
  | Embarrassment
  | Discomfiture
  | Awkwardness
  deriving stock (Show, Eq, Ord)

data InterestSubtype
  = Interest
  | Curiosity
  deriving stock (Show, Eq, Ord)

data RageEmotion = RageEmotion
data FearEmotion = FearEmotion
data JoyEmotion = JoyEmotion
data SadnessEmotion = SadnessEmotion
data SurpriseEmotion = SurpriseEmotion
data ShameEmotion = ShameEmotion
data InterestEmotion = InterestEmotion

instance Show RageEmotion where
  show RageEmotion = "Rage"

instance Show FearEmotion where
  show FearEmotion = "Fear"

instance Show JoyEmotion where
  show JoyEmotion = "Joy"

instance Show SadnessEmotion where
  show SadnessEmotion = "Sadness"

instance Show SurpriseEmotion where
  show SurpriseEmotion = "Surprise"

instance Show ShameEmotion where
  show ShameEmotion = "Shame"

instance Show InterestEmotion where
  show InterestEmotion = "Interest"

class Emotion emotion where
  type Subtype emotion :: Type

  emotionSubtypes :: Set (Subtype emotion)

  showSubtypes :: [Text]

  default showSubtypes :: (a ~ Subtype emotion, Show a) => [Text]
  showSubtypes = S.toList (emotionSubtypes @emotion) <&> T.pack . show

  parseSubtype :: Text -> Maybe (Subtype emotion)
  parseSubtype txt = keyValue M.!? txt
    where
      keyValue = M.fromList $ zip (showSubtypes @emotion) (S.toList $ emotionSubtypes @emotion)

instance Emotion RageEmotion where
  type Subtype RageEmotion = RageSubtype
  emotionSubtypes =
    [ Rabies
    , Fury
    , Anger
    , Hatred
    , Disgust
    , Disturbance
    , Indignation
    , Wrath
    , Irritation
    , Contempt
    , Dislike
    , Vulnerability
    , Annoyance
    ]

instance Emotion FearEmotion where
  type Subtype FearEmotion = FearSubtype
  emotionSubtypes =
    [ Horror
    , Fright
    , Fear
    , Anxiety
    , Worry
    , Apprehension
    , Perplexity
    , Timidity
    ]

instance Emotion JoyEmotion where
  type Subtype JoyEmotion = JoySubtype
  emotionSubtypes =
    [ Delight
    , Admiration
    , Rapture
    , Exultation
    , Happiness
    , Joy
    , Elation
    , Appeasement
    , Tenderness
    , Pride
    , Gratitude
    , Inspiration
    , Pleasure
    ]

instance Emotion SadnessEmotion where
  type Subtype SadnessEmotion = SadnessSubtype
  emotionSubtypes =
    [ Sorrow
    , Grief
    , Despair
    , Yearning
    , Resentment
    , Emptiness
    , Melancholy
    , Sadness
    , Despondency
    , Helplessness
    , Regret
    , Weakness
    , Depression
    ]

instance Emotion SurpriseEmotion where
  type Subtype SurpriseEmotion = SurpriseSubtype
  emotionSubtypes =
    [ Stun
    , Amazement
    , Astonishment
    , Confusion
    ]

instance Emotion ShameEmotion where
  type Subtype ShameEmotion = ShameSubtype
  emotionSubtypes =
    [ Cringe
    , Embarrassment
    , Discomfiture
    , Awkwardness
    ]

instance Emotion InterestEmotion where
  type Subtype InterestEmotion = InterestSubtype
  emotionSubtypes =
    [ Interest
    , Curiosity
    ]

data TrackedEmotion where
  TrackedEmotion :: (Emotion a, Show a, b ~ Subtype a, Show b) => a -> Subtype a -> TrackedEmotion

data SomeEmotion where
  SomeEmotion :: (Emotion a, Show a, b ~ Subtype a, Show b) => a -> SomeEmotion

data UserEmotion = UserEmotion
  { ueTrackedEmotion :: TrackedEmotion
  , ueComment :: Maybe Text
  , ueTime :: UTCTime
  }
makeLensesWith abbreviatedFields ''UserEmotion

instance ToNamedRecord UserEmotion where
  toNamedRecord (UserEmotion (TrackedEmotion emotion subtype) mComment time) =
    namedRecord
      [ "Emotion" .= show emotion
      , "Emotion subtype" .= show subtype
      , "Comment" .= comment
      , "Time" .= show time
      ]
    where
      comment = fromMaybe "" mComment

emotionsHeader :: Header
emotionsHeader =
  header
    [ "Emotion"
    , "Emotion subtype"
    , "Comment"
    , "Time"
    ]
