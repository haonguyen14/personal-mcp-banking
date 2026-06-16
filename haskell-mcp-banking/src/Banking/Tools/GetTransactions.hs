{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Banking.Tools.GetTransactions (GetTransactionsTool (..)) where

import Banking.Plaid (PlaidConfig, getTransactions)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON (..), Value, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Tool

data GetTransactionsTool = GetTransactionsTool PlaidConfig (TVar (Maybe Text))

data GetTransactionsInput = GetTransactionsInput
  { startDate :: Text,
    endDate :: Text
  }

instance FromJSON GetTransactionsInput where
  parseJSON = withObject "GetTransactionsInput" $ \o ->
    GetTransactionsInput
      <$> o .: "startDate"
      <*> o .: "endDate"

data Transaction = Transaction
  { txId :: Text,
    txDate :: Text,
    txAmount :: Double,
    txName :: Text,
    txMerchantName :: Maybe Text,
    txCategory :: Maybe [Text],
    txPending :: Bool
  }

parseTransactions :: Value -> Maybe [Transaction]
parseTransactions = Aeson.parseMaybe $
  withObject "transactions/get" $ \obj -> do
    txList <- obj .: "transactions"
    mapM parseTx txList
  where
    parseTx = withObject "transaction" $ \obj ->
      Transaction
        <$> obj .: "transaction_id"
        <*> obj .: "date"
        <*> obj .: "amount"
        <*> obj .: "name"
        <*> obj .:? "merchant_name"
        <*> obj .:? "category"
        <*> obj .: "pending"

instance ToolCapability GetTransactionsTool where
  type Input GetTransactionsTool = GetTransactionsInput

  getMetadata _ =
    ToolMetadata
      { name = "get_transactions",
        title = Just "Get Transactions",
        description = Just "Fetch bank transactions for a given date range (YYYY-MM-DD)",
        inputSchema = schema
      }

  runTool (GetTransactionsTool cfg tokenVar) input = do
    tok <- readTVarIO tokenVar
    case tok of
      Nothing -> return $ ToolResult [ToolText notConnectedMsg] True
      Just accessToken -> do
        result <- runExceptT $ getTransactions cfg accessToken (startDate input) (endDate input)
        case result of
          Left err -> return $ ToolResult [ToolText $ T.pack (show err)] True
          Right val -> case parseTransactions val of
            Nothing -> return $ ToolResult [ToolText "Failed to parse transactions from Plaid response"] True
            Just txs -> return $ ToolResult [ToolText (formatTransactions txs)] False

notConnectedMsg :: Text
notConnectedMsg = "Bank account not connected. Open https://localhost:5173 in your browser to complete the Plaid Link flow."

formatTransactions :: [Transaction] -> Text
formatTransactions [] = "No transactions found for the given date range."
formatTransactions txs = T.intercalate "\n" (map formatTx txs)
  where
    formatTx tx =
      T.intercalate " | " $
        filter
          (not . T.null)
          [ txDate tx,
            txName tx <> " ($" <> T.pack (show (txAmount tx)) <> ")",
            maybe "" (T.intercalate " > ") (txCategory tx),
            if txPending tx then "(pending)" else ""
          ]

schema :: Value
schema =
  object
    [ "type" .= ("object" :: Text),
      "required" .= (["startDate", "endDate"] :: [Text]),
      "properties"
        .= object
          [ "startDate" .= object ["type" .= ("string" :: Text), "description" .= ("Start date in YYYY-MM-DD format" :: Text)],
            "endDate" .= object ["type" .= ("string" :: Text), "description" .= ("End date in YYYY-MM-DD format" :: Text)]
          ]
    ]
