{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import API (API, bankingServer)
import Control.Concurrent.STM (newTVarIO)
import McpTypes (ServerInfo (..))
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Plaid (PlaidConfig (..))
import Servant (Proxy (..), serve, (:<|>) (..))
import Server (MCPRegistry (..), mcpServer)
import System.Environment (getEnv)
import Tool (SomeTool (..))
import Tools.GetTransactions (GetTransactionsTool (..))

main :: IO ()
main = do
  clientId <- getEnv "PLAID_CLIENT_ID"
  secret <- getEnv "PLAID_SECRET"
  env <- getEnv "PLAID_ENV"
  redirectUri <- getEnv "PLAID_REDIRECT_URI"

  let cfg =
        PlaidConfig
          { plaidClientId = clientId,
            plaidSecret = secret,
            plaidEnv = env,
            plaidRedirectUri = redirectUri
          }

  tokenVar <- newTVarIO Nothing

  let registry =
        MCPRegistry
          { serverInfo =
              ServerInfo
                { serverName = "haskell-mcp-banking",
                  serverTitle = "Banking MCP Server",
                  serverVersion = "0.1.0.0",
                  serverDescription = "MCP server for personal finance via Plaid"
                },
            serverInstruction = "Send JSON-RPC requests to /mcp",
            prompts = [],
            tools = [SomeTool (GetTransactionsTool cfg tokenVar)],
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
