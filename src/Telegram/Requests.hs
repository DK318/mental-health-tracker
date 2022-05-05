module Telegram.Requests where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH
import Telegram.Types
import Telegram.JsonExt
import Data.ByteString.Lazy (ByteString)
import Servant.Multipart.Client
import Servant.Multipart.API (Mem, MultipartData (MultipartData), Input (Input), FileData (FileData))
import qualified Data.Text as T

data GetUpdatesRequest = GetUpdatesRequest
  { gur_offset :: Maybe Integer
  , gur_limit :: Maybe Integer
  , gur_timeout :: Maybe Integer
  , gur_allowed_updates :: Maybe [Text]
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 4) ''GetUpdatesRequest

mkUpdatesRequest :: Integer -> GetUpdatesRequest
mkUpdatesRequest offset = GetUpdatesRequest (Just offset) Nothing Nothing Nothing

data SendMessageRequest = SendMessageRequest
  { smr_chat_id :: Integer
  , smr_text :: Text
  , smr_reply_markup :: Maybe ReplyKeyboard
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 4) ''SendMessageRequest

data SendDocumentRequest = SendDocumentRequest
  { sdr_chat_id :: Integer
  , sdr_document :: ByteString
  }
  deriving stock (Show, Generic)

instance ToMultipart Mem SendDocumentRequest where
  toMultipart (SendDocumentRequest chatId contents) =
    MultipartData [Input "chat_id" $ (T.pack . show) chatId] [FileData "document" "data.csv" "text/csv" contents]
