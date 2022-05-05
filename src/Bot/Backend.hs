module Bot.Backend where

import Control.Lens
import Polysemy (Members, Embed, Final, Sem, embed, Member)
import Polysemy.Error (Error, throw, runError)
import Bot.Error (BotError (NoSuchEmotion, EmotionSubtypeParseError, NoUserEmotions))
import Polysemy.State (State, get, put, modify)
import Bot.State
import Servant.Client (ClientM)
import Telegram.Types
import Telegram.Requests (SendMessageRequest(SendMessageRequest), mkUpdatesRequest, SendDocumentRequest (SendDocumentRequest))
import Telegram.API
import Control.Monad (void, forM_)
import Bot.Emotions
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Time (getCurrentTime)
import Control.Monad.Extra (whenJust)
import Data.Either (fromRight)
import Control.Lens.Extras (is)
import Data.Csv (encodeByName)
import Servant.Multipart.Client (genBoundary)

type Effects r = Members '[State BotState, Embed ClientM, Embed IO, Final IO] r

data ReturnExc = ReturnExc

collectUpdates :: (Effects r) => Sem r [Update]
collectUpdates = do
  botState <- get @BotState
  let getUpdatesRequest = mkUpdatesRequest (botState ^. offset)
  updates <- embed $ getUpdates (botState ^. token) getUpdatesRequest
  pure $ result updates

processUpdates :: (Effects r) => [Update] -> Sem r ()
processUpdates updates =
  processUpdates_ updates
    & runError @ReturnExc
    <&> fromRight ()

processUpdates_ :: forall r. (Effects r, Member (Error ReturnExc) r) => [Update] -> Sem r ()
processUpdates_ updates = do
  forM_ updates \update -> do
    modify @BotState \botState -> botState & offset .~ update ^. update_update_id + 1
    let mUser = update ^? update_message . _Just . message_from . _Just
    let mText = update ^? update_message . _Just . message_text . _Just
    whenJust mUser \user -> do
      whenJust mText \text -> do
        let userId = user ^. user_id
        if
          | T.isPrefixOf "/track" text -> do
            modify @BotState \botState ->
              botState
                & asked . at userId .~ Nothing
                & someEmotions . at userId .~ Nothing
                & tracked . at userId .~ Nothing
            askForEmotion user
          | T.isPrefixOf "/stats" text -> do
            handle userId
              do sendStats user
              do id
          | otherwise -> do
            botState <- get @BotState

            let mAsked = botState ^. asked . at userId
            whenJust mAsked \_ -> do
              handle userId
                do askForSubtype user text
                do \botState -> botState & asked . at userId .~ Nothing
              throw ReturnExc

            let mSomeEmotion = botState ^. someEmotions . at userId
            whenJust mSomeEmotion \someEmotion -> do
              handle userId
                do makeEmotion someEmotion user text
                do \botState -> botState & someEmotions . at userId .~ Nothing
              throw ReturnExc

            let mTracked = botState ^. tracked . at userId
            whenJust mTracked \trackedEmotion -> do
              getComment trackedEmotion user text
              modify @BotState \botState -> botState & tracked . at userId .~ Nothing
              throw ReturnExc
  where
    handle :: Integer -> Sem (Error BotError ': r) () -> (BotState -> BotState) -> Sem r ()
    handle userId action onSuccess = do
      runError action >>= \case
        Right () -> modify onSuccess
        Left err -> do
          let sendRequest = SendMessageRequest userId (T.pack $ "Error " <> show err) Nothing
          tkn <- get @BotState <&> view token
          void $ embed $ sendMessage tkn sendRequest

sendStats :: (Effects r, Member (Error BotError) r) => User -> Sem r ()
sendStats user = do
  botState <- get @BotState

  let mUserEmotions = botState ^. database . at (user ^. user_id)
  if _Just `is` mUserEmotions
    then do
      let userEmotions = mUserEmotions ^?! _Just
      let csv = encodeByName emotionsHeader userEmotions
      boundary <- embed genBoundary
      let sendRequest = SendDocumentRequest (user ^. user_id) csv
      void $ embed $ sendDocument (botState ^. token) (boundary, sendRequest)
    else throw NoUserEmotions

askForEmotion :: (Effects r) => User -> Sem r ()
askForEmotion user = do
  let buttons =
        [ ["Rage"]
        , ["Fear"]
        , ["Joy"]
        , ["Sadness"]
        , ["Surprise"]
        , ["Shame"]
        , ["Interest"]
        ]

  let keyboard = ReplyKeyboardMarkup buttons Nothing (Just True) (Just "Emotion") (Just True)
  let sendRequest = SendMessageRequest (user ^. user_id) "Choose emotion:" (Just keyboard)

  botState <- get @BotState
  let botState' =
        botState
          & asked . at (user ^. user_id) ?~ True

  let tkn = botState ^. token

  void $ embed $ sendMessage tkn sendRequest
  put botState'

askForSubtype :: forall r. (Effects r, Member (Error BotError) r) => User -> Text -> Sem r ()
askForSubtype user text = do
  (emotion, labels) <- getEmotion text

  let buttons = [[KeyboardButton label] | label <- labels]
  let keyboard = ReplyKeyboardMarkup buttons Nothing (Just True) (Just "Emotion subtype") (Just True)
  let sendRequest = SendMessageRequest (user ^. user_id) "Choose emotion subtype:" (Just keyboard)

  botState <- get @BotState

  void $ embed $ sendMessage (botState ^. token) sendRequest

  let botState' =
        botState
          & someEmotions . at (user ^. user_id) ?~ emotion

  put botState'
  where
    getEmotion :: Text -> Sem r (SomeEmotion, [Text])
    getEmotion = \case
      "Rage" -> pure (SomeEmotion RageEmotion, showSubtypes @RageEmotion)
      "Fear" -> pure (SomeEmotion FearEmotion, showSubtypes @FearEmotion)
      "Joy" -> pure (SomeEmotion JoyEmotion, showSubtypes @JoyEmotion)
      "Sadness" -> pure (SomeEmotion SadnessEmotion, showSubtypes @SadnessEmotion)
      "Surprise" -> pure (SomeEmotion SurpriseEmotion, showSubtypes @SurpriseEmotion)
      "Shame" -> pure (SomeEmotion ShameEmotion, showSubtypes @ShameEmotion)
      "Interest" -> pure (SomeEmotion InterestEmotion, showSubtypes @InterestEmotion)
      _ -> throw NoSuchEmotion

makeEmotion :: (Effects r, (Member (Error BotError) r)) => SomeEmotion -> User -> Text -> Sem r ()
makeEmotion (SomeEmotion (emotion :: a)) user text = do
  botState <- get @BotState
  subtype <- maybe (throw EmotionSubtypeParseError) pure (parseSubtype @a text)

  let trackedEmotion = TrackedEmotion emotion subtype

  let deleteKeyboard = ReplyKeyboardRemove True (Just True)

  let sendRequest = SendMessageRequest (user ^. user_id) "Enter comment. If you don't want pass dot instead (.)" (Just deleteKeyboard)
  void $ embed $ sendMessage (botState ^. token) sendRequest

  let botState' =
        botState
          & tracked . at (user ^. user_id) ?~ trackedEmotion

  put botState'

getComment :: (Effects r) => TrackedEmotion -> User -> Text -> Sem r ()
getComment trackedEmotion user text = do
  let mText = if text == "." then Nothing else Just text

  botState <- get @BotState

  let sendRequest = SendMessageRequest (user ^. user_id) "Emotion tracked!" Nothing
  void $ embed $ sendMessage (botState ^. token) sendRequest

  now <- embed getCurrentTime

  let userEmotion = UserEmotion trackedEmotion mText now

  let botState' =
        botState
          & database %~ M.insertWith (++) (user ^. user_id) [userEmotion]

  put botState'
