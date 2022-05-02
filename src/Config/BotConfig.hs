module Config.BotConfig where

import Toml (TomlCodec, (.=))
import Telegram.API (Token(Token, unToken))
import qualified Toml

tokenCodec :: TomlCodec Token
tokenCodec =
  Token
    <$> Toml.text "token" .= unToken
