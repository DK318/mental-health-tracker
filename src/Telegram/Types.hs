module Telegram.Types where

import Control.Lens
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson.TH
import GHC.Exts (IsString)
import Telegram.JsonExt

data ResponseParameters = ResponseParameters
  { rp_migrate_to_char_id :: Maybe Integer
  , rp_retry_after :: Maybe Integer
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 3) ''ResponseParameters

data Response a = Response
  { result :: a
  , parameters :: Maybe ResponseParameters
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 0) ''Response

data User = User
  { _user_id :: Integer
  , _user_is_bot :: Bool
  , _user_first_name :: Text
  , _user_last_name :: Maybe Text
  , _user_username :: Maybe Text
  , _user_language_code :: Maybe Text
  , _user_can_join_groups :: Maybe Bool
  , _user_can_read_all_group_messages :: Maybe Bool
  , _user_supports_inline_queries :: Maybe Bool
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 6) ''User
makeLenses ''User

data Chat = Chat
  { chat_id :: Integer
  , chat_type :: Text
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 5) ''Chat

newtype KeyboardButton = KeyboardButton { kb_text :: Text }
  deriving stock (Show, Generic)
  deriving newtype (IsString)
deriveJSON (dropOptions 3) ''KeyboardButton

data ReplyKeyboardMarkup = ReplyKeyboardMarkup
  { rkm_keyboard :: [[KeyboardButton]]
  , rkm_resize_keyboard :: Maybe Bool
  , rkm_one_time_keyboard :: Maybe Bool
  , rkm_input_field_placeholder :: Maybe Text
  , rkm_selective :: Maybe Bool
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 4) ''ReplyKeyboardMarkup

data Message = Message
  { _message_message_id :: Integer
  , _message_from :: Maybe User
  , _message_date :: Integer
  , _message_chat :: Chat
  , _message_text :: Maybe Text
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 9) ''Message
makeLenses ''Message

data Update = Update
  { _update_update_id :: Integer
  , _update_message :: Maybe Message
  }
  deriving stock (Show, Generic)
deriveJSON (dropOptions 8) ''Update
makeLenses ''Update
