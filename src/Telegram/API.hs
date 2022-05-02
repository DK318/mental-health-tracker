module Telegram.API where

import Servant.API
import Data.Text (Text)
import Telegram.Requests (GetUpdatesRequest, SendMessageRequest, SendDocumentRequest)
import Data.Data (Proxy (Proxy))
import Servant.Client (ClientM, client)
import Telegram.Response (GetUpdatesResponse, SendMessageResponse)
import Servant.Multipart.API (MultipartForm, Mem)
import Data.ByteString.Lazy (ByteString)

newtype Token = Token { unToken :: Text }
  deriving newtype (Show, Eq, ToHttpApiData)

type TelegramToken = Capture ":token" Token

type GetUpdatesAPI =
  TelegramToken :> "getUpdates"
  :> ReqBody '[JSON] GetUpdatesRequest
  :> Get '[JSON] GetUpdatesResponse

type SendMessageAPI =
  TelegramToken :> "sendMessage"
  :> ReqBody '[JSON] SendMessageRequest
  :> Post '[JSON] SendMessageResponse

type SendDocumentAPI =
  TelegramToken :> "sendDocument"
  :> MultipartForm Mem SendDocumentRequest
  :> Post '[JSON] SendMessageResponse

getUpdatesProxy :: Proxy GetUpdatesAPI
getUpdatesProxy = Proxy

getUpdates :: Token -> GetUpdatesRequest -> ClientM GetUpdatesResponse
getUpdates = client getUpdatesProxy

sendMessageProxy :: Proxy SendMessageAPI
sendMessageProxy = Proxy

sendMessage :: Token -> SendMessageRequest -> ClientM SendMessageResponse
sendMessage = client sendMessageProxy

sendDocumentProxy :: Proxy SendDocumentAPI
sendDocumentProxy = Proxy

sendDocument :: Token -> (ByteString, SendDocumentRequest) -> ClientM SendMessageResponse
sendDocument = client sendDocumentProxy
