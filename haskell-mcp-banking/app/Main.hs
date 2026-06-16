{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Banking.API (API, bankingServer)
import Banking.Plaid (PlaidConfig (..))
import Banking.Tools.GetTransactions (GetTransactionsTool (..))
import Control.Concurrent.STM (newTVarIO)
import Data.Text (pack)
import qualified Data.Text.Encoding as TE
import Email.Tools.ArchiveEmails (ArchiveEmailsTool (..))
import Email.Tools.GetEmails (GetEmailsTool (..))
import McpTypes (ServerInfo (..))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Servant (Proxy (..), serve, (:<|>) (..))
import Server (MCPRegistry (..), mcpServer)
import System.Environment (getEnv, lookupEnv)
import Tool (SomeTool (..))

main :: IO ()
main = do
  clientId <- getEnv "PLAID_CLIENT_ID"
  secret <- getEnv "PLAID_SECRET"
  env <- getEnv "PLAID_ENV"
  redirectUri <- getEnv "PLAID_REDIRECT_URI"
  accessToken <- (pack <$>) <$> lookupEnv "PLAID_ACCESS_TOKEN"
  fastmailToken <- TE.encodeUtf8 . pack <$> getEnv "FASTMAIL_API_TOKEN"

  let cfg =
        PlaidConfig
          { plaidClientId = clientId,
            plaidSecret = secret,
            plaidEnv = env,
            plaidRedirectUri = redirectUri
          }

  tokenVar <- newTVarIO accessToken

  let registry =
        MCPRegistry
          { serverInfo =
              ServerInfo
                { serverName = "haskell-mcp-personal",
                  serverTitle = "Personal Assistant MCP Server",
                  serverVersion = "0.1.0.0",
                  serverDescription = "MCP server for personal finance and email management"
                },
            serverInstruction = "Send JSON-RPC requests to /mcp",
            prompts = [],
            tools =
              [ SomeTool (GetTransactionsTool cfg tokenVar),
                SomeTool (GetEmailsTool fastmailToken),
                SomeTool (ArchiveEmailsTool fastmailToken)
              ],
            resources = []
          }

  let port = 8080
  putStrLn $ "Starting on port " <> show port
  run port $
    cors (const $ Just corsPolicy) $
      serve
        (Proxy :: Proxy API)
        (mcpServer registry :<|> bankingServer cfg tokenVar)

corsPolicy :: CorsResourcePolicy
corsPolicy =
  simpleCorsResourcePolicy
    { corsOrigins = Just (["https://localhost:5173"], True),
      corsMethods = ["GET", "POST", "OPTIONS"],
      corsRequestHeaders = ["Content-Type"]
    }
