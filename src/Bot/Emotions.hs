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
import Data.Csv (ToRecord (toRecord), record, ToField (toField))
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
  deriving stock (Eq, Ord)

instance Show RageSubtype where
  show = \case
    Rabies -> "Бешенство"
    Fury -> "Ярость"
    Anger -> "Злость"
    Hatred -> "Ненависть"
    Disgust -> "Отвращение"
    Disturbance -> "Возмущение"
    Indignation -> "Негодование"
    Wrath -> "Гнев"
    Irritation -> "Раздражение"
    Contempt -> "Презрение"
    Dislike -> "Неприязнь"
    Vulnerability -> "Уязвленность"
    Annoyance -> "Досада"

data FearSubtype
  = Horror
  | Fright
  | Fear
  | Anxiety
  | Worry
  | Apprehension
  | Perplexity
  | Timidity
  deriving stock (Eq, Ord)

instance Show FearSubtype where
  show = \case
    Horror -> "Ужас"
    Fright -> "Испуг"
    Fear -> "Страх"
    Anxiety -> "Тревога"
    Worry -> "Беспокойство"
    Apprehension -> "Опасение"
    Perplexity -> "Растерянность"
    Timidity -> "Робость"

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
  deriving stock (Eq, Ord)

instance Show JoySubtype where
  show = \case
    Delight -> "Восторг"
    Admiration -> "Восхищение"
    Rapture -> "Упоение"
    Exultation -> "Ликование"
    Happiness -> "Счастье"
    Joy -> "Радость"
    Elation -> "Приподнятость"
    Appeasement -> "Умиротворение"
    Tenderness -> "Нежность"
    Pride -> "Гордость"
    Gratitude -> "Благодарность"
    Inspiration -> "Вдохновение"
    Pleasure -> "Наслаждение"

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
  deriving stock (Eq, Ord)

instance Show SadnessSubtype where
  show = \case
    Sorrow -> "Скорбь"
    Grief -> "Горе"
    Despair -> "Отчаяние"
    Yearning -> "Тоска"
    Resentment -> "Обида"
    Emptiness -> "Опустошенность"
    Melancholy -> "Грусть"
    Sadness -> "Печаль"
    Despondency -> "Уныние"
    Helplessness -> "Беспомощность"
    Regret -> "Сожаление"
    Weakness -> "Бессилие"
    Depression -> "Подавленность"

data SurpriseSubtype
  = Stun
  | Amazement
  | Astonishment
  | Confusion
  deriving stock (Eq, Ord)

instance Show SurpriseSubtype where
  show = \case
    Stun -> "Оторопь"
    Amazement -> "Изумление"
    Astonishment -> "Удивление"
    Confusion -> "Замешательство"

data ShameSubtype
  = Cringe
  | Embarrassment
  | Discomfiture
  | Awkwardness
  deriving stock (Eq, Ord)

instance Show ShameSubtype where
  show = \case
    Cringe -> "Стыд"
    Embarrassment -> "Конфуз"
    Discomfiture -> "Смущение"
    Awkwardness -> "Неловкость"

data InterestSubtype
  = Interest
  | Curiosity
  deriving stock (Eq, Ord)

instance Show InterestSubtype where
  show = \case
    Interest -> "Интерес"
    Curiosity -> "Любопытство"

data RageEmotion = RageEmotion
data FearEmotion = FearEmotion
data JoyEmotion = JoyEmotion
data SadnessEmotion = SadnessEmotion
data SurpriseEmotion = SurpriseEmotion
data ShameEmotion = ShameEmotion
data InterestEmotion = InterestEmotion

instance Show RageEmotion where
  show RageEmotion = "Злость"

instance Show FearEmotion where
  show FearEmotion = "Страх"

instance Show JoyEmotion where
  show JoyEmotion = "Радость"

instance Show SadnessEmotion where
  show SadnessEmotion = "Грусть"

instance Show SurpriseEmotion where
  show SurpriseEmotion = "Удивление"

instance Show ShameEmotion where
  show ShameEmotion = "Стыд"

instance Show InterestEmotion where
  show InterestEmotion = "Интерес"

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

instance ToRecord UserEmotion where
  toRecord (UserEmotion (TrackedEmotion emotion subtype) mComment time) =
    record
      [ toField $ show emotion
      , toField $ show subtype
      , toField comment
      , toField $ show time
      ]
    where
      comment = fromMaybe "" mComment
