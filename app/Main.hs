module Main where

import Servant.Client.Core (BaseUrl (BaseUrl), Scheme (Https))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Telegram.API (Token(..))
import Servant.Client (mkClientEnv, runClientM, ClientM, ClientEnv)
import Polysemy
import Polysemy.State
import Bot.State
import Data.Function ((&))
import Polysemy.Embed (runEmbedded)
import System.Exit
import Fmt (pretty)
import Bot.Backend (collectUpdates, processUpdates)
import System.Time.Extra (sleep)
import qualified Data.Text.IO as TIO
import Config.BotConfig (tokenCodec)
import Toml (decode, prettyTomlDecodeErrors)
import qualified Data.Text as T

telegramBaseUrl :: BaseUrl
telegramBaseUrl = BaseUrl Https "api.telegram.org" 443 ""

getToken :: IO Token
getToken = do
  configTxt <- TIO.readFile "config.toml"
  case decode tokenCodec configTxt of
    Left err -> die $ T.unpack $ prettyTomlDecodeErrors err
    Right val -> pure val { unToken = "bot" <> unToken val }

runEffects
  :: ClientEnv
  -> BotState
  -> Sem '[State BotState, Embed ClientM, Embed IO, Final IO] a
  -> IO BotState
runEffects env botState action =
  execState botState action
    & runEmbedded runClientP
    & embedToFinal @IO
    & runFinal
    where
      runClientP :: ClientM x -> IO x
      runClientP act =
        runClientM act env >>= \case
          Left err -> die $ pretty (show err)
          Right val -> pure val

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  token <- getToken
  let clientEnv = mkClientEnv manager telegramBaseUrl
  let botState = initBotState token
  loop clientEnv botState
  where
    loop :: ClientEnv -> BotState -> IO ()
    loop env botState = do
      botState' <-
        runEffects env botState do
          updates <- collectUpdates
          processUpdates updates
      sleep 1
      loop env botState'
