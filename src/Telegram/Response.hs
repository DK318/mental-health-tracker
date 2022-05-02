module Telegram.Response where

import Telegram.Types (Update, Response, Message)

type GetUpdatesResponse = Response [Update]

type SendMessageResponse = Response Message
