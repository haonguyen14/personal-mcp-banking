{-# LANGUAGE OverloadedStrings #-}

module Plaid
  ( PlaidConfig (..),
    PlaidError (..),
    createLinkToken,
    exchangePublicToken,
    getTransactions,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)

data PlaidConfig = PlaidConfig
  { plaidClientId :: String,
    plaidSecret :: String,
    plaidEnv :: String, -- TODO: extract enum type
    plaidRedirectUri :: String
  }

data PlaidError
  = PlaidHttpError String
  | PlaidParseError String
  | PlaidMissingField String
  | PlaidApiError Text Text -- error_code, error_message
  deriving (Show)

baseUrl :: PlaidConfig -> String
baseUrl cfg = "https://" <> plaidEnv cfg <> ".plaid.com"

defaultUserId :: Text
defaultUserId = "default-user"

plaidPost :: PlaidConfig -> String -> Aeson.Value -> ExceptT PlaidError IO Aeson.Value
plaidPost cfg endpoint body = do
  manager <- liftIO newTlsManager
  result <- liftIO $ try $ do
    initReq <- parseRequest (baseUrl cfg <> endpoint)
    let req =
          initReq
            { method = "POST",
              requestBody = RequestBodyLBS (Aeson.encode body),
              requestHeaders = [("Content-Type", "application/json")]
            }
    httpLbs req manager
  case result of
    Left err -> throwError $ PlaidHttpError (show (err :: SomeException))
    Right response -> case Aeson.eitherDecode (responseBody response) of
      Left err -> throwError $ PlaidParseError err
      Right val -> case Aeson.parseMaybe (Aeson.withObject "error" parseApiError) val of
        Just err -> throwError err
        Nothing -> return val
  where
    parseApiError obj =
      PlaidApiError
        <$> obj .: "error_code"
        <*> obj .: "error_message"

createLinkToken :: PlaidConfig -> ExceptT PlaidError IO Text
createLinkToken cfg = do
  val <-
    plaidPost cfg "/link/token/create" $
      Aeson.object
        [ "client_id" .= plaidClientId cfg,
          "secret" .= plaidSecret cfg,
          "client_name" .= ("mcp-banking" :: Text),
          -- TODO: hardcoded to single user; update client_user_id when multi-user support is added
          "user" .= Aeson.object ["client_user_id" .= (defaultUserId :: Text)],
          "products" .= (["transactions"] :: [Text]),
          "country_codes" .= (["US"] :: [Text]),
          "language" .= ("en" :: Text),
          "redirect_uri" .= plaidRedirectUri cfg
        ]
  case Aeson.parseMaybe (Aeson.withObject "link/token/create" (.: "link_token")) val of
    Nothing -> throwError $ PlaidMissingField "link_token"
    Just tok -> return tok

exchangePublicToken :: PlaidConfig -> Text -> ExceptT PlaidError IO Text
exchangePublicToken cfg publicTkn = do
  val <-
    plaidPost cfg "/item/public_token/exchange" $
      Aeson.object
        [ "client_id" .= plaidClientId cfg,
          "secret" .= plaidSecret cfg,
          "public_token" .= publicTkn
        ]
  case Aeson.parseMaybe (Aeson.withObject "item/public_token/exchange" (.: "access_token")) val of
    Nothing -> throwError $ PlaidMissingField "access_token"
    Just tok -> return tok

getTransactions :: PlaidConfig -> Text -> Text -> Text -> ExceptT PlaidError IO Aeson.Value
getTransactions cfg accessToken startDate endDate =
  plaidPost cfg "/transactions/get" $
    Aeson.object
      [ "client_id" .= plaidClientId cfg,
        "secret" .= plaidSecret cfg,
        "access_token" .= accessToken,
        "start_date" .= startDate,
        "end_date" .= endDate,
        "options" .= Aeson.object ["count" .= (100 :: Int)]
      ]
