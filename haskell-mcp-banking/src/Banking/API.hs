{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Banking.API
  ( API,
    BankingAPI,
    LinkTokenResponse (..),
    ExchangeTokenRequest (..),
    ConnectionStatus (..),
    bankingServer,
  )
where

import Banking.Plaid (PlaidConfig, PlaidError (..), createLinkToken, exchangePublicToken)
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant
import Server (JSONRpc)

data LinkTokenResponse = LinkTokenResponse
  { linkToken :: Text
  }
  deriving (Generic, ToJSON)

data ExchangeTokenRequest = ExchangeTokenRequest
  { publicToken :: Text
  }
  deriving (Generic, FromJSON)

data ConnectionStatus = ConnectionStatus
  { connected :: Bool
  }
  deriving (Generic, ToJSON)

type BankingAPI =
  "link-token" :> Post '[JSON] LinkTokenResponse
    :<|> "exchange-token" :> ReqBody '[JSON] ExchangeTokenRequest :> Post '[JSON] NoContent
    :<|> "check-connection" :> Post '[JSON] ConnectionStatus

type API = JSONRpc :<|> BankingAPI

bankingServer :: PlaidConfig -> TVar (Maybe Text) -> Server BankingAPI
bankingServer cfg tokenVar =
  handleLinkToken cfg
    :<|> handleExchangeToken cfg tokenVar
    :<|> handleCheckConnection tokenVar

handleLinkToken :: PlaidConfig -> Handler LinkTokenResponse
handleLinkToken cfg = do
  result <- liftIO $ runExceptT $ createLinkToken cfg
  case result of
    Left err -> throwError $ toServantError err
    Right tok -> return $ LinkTokenResponse tok

handleExchangeToken :: PlaidConfig -> TVar (Maybe Text) -> ExchangeTokenRequest -> Handler NoContent
handleExchangeToken cfg tokenVar req = do
  result <- liftIO $ runExceptT $ exchangePublicToken cfg (publicToken req)
  case result of
    Left err -> throwError $ toServantError err
    Right accessToken -> do
      liftIO $ do
        print accessToken
        atomically $ writeTVar tokenVar (Just accessToken)
      return NoContent

handleCheckConnection :: TVar (Maybe Text) -> Handler ConnectionStatus
handleCheckConnection tokenVar = do
  tok <- liftIO $ readTVarIO tokenVar
  return $ ConnectionStatus (tok /= Nothing)

toServantError :: PlaidError -> ServerError
toServantError (PlaidApiError code msg) =
  err400 {errBody = BSL.pack $ "Plaid error " <> show code <> ": " <> show msg}
toServantError (PlaidHttpError msg) =
  err503 {errBody = BSL.pack $ "Could not reach Plaid: " <> msg}
toServantError (PlaidParseError msg) =
  err502 {errBody = BSL.pack $ "Invalid response from Plaid: " <> msg}
toServantError (PlaidMissingField field) =
  err502 {errBody = BSL.pack $ "Missing field in Plaid response: " <> field}
