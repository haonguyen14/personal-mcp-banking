{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Email.Tools.GetEmails (GetEmailsTool (..)) where

import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson (FromJSON (..), Value, object, withObject, (.:?), (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Email.Fastmail (Email (..), EmailAddress, getEmails, getMailboxIds, getSession, retrieveMailbox)
import qualified Email.Fastmail as Fastmail
import Tool

data GetEmailsTool = GetEmailsTool BS.ByteString

data GetEmailsInput = GetEmailsInput
  { start :: Integer,
    limit :: Integer
  }

instance FromJSON GetEmailsInput where
  parseJSON = withObject "GetEmailsInput" $ \o ->
    GetEmailsInput
      <$> (maybe 0 id <$> o .:? "start")
      <*> (maybe 20 id <$> o .:? "limit")

instance ToolCapability GetEmailsTool where
  type Input GetEmailsTool = GetEmailsInput

  getMetadata _ =
    ToolMetadata
      { name = "get_emails",
        title = Just "Get Emails",
        description = Just "Fetch emails from inbox with subject and body. Use start/limit for pagination.",
        inputSchema = schema
      }

  runTool (GetEmailsTool apiToken) input = do
    result <- runExceptT $ do
      session <- getSession apiToken
      mailboxIds <- getMailboxIds session apiToken "Inbox"
      case mailboxIds of
        [] -> throwE "Inbox mailbox not found"
        (inboxId : _) -> do
          let page = (start input, limit input)
          emailIds <- retrieveMailbox page session apiToken inboxId
          getEmails session apiToken emailIds
    case result of
      Left err -> return $ ToolResult [ToolText $ T.pack err] True
      Right emails -> return $ ToolResult [ToolText (formatEmails emails)] False

formatEmails :: [Email] -> Text
formatEmails [] = "No emails found."
formatEmails emails = T.intercalate "\n\n---\n\n" (map formatEmail emails)

formatEmail :: Email -> Text
formatEmail e =
  T.unlines
    [ "ID: " <> T.pack (emailId e),
      "From: " <> formatSenders (from e),
      "Subject: " <> subject e,
      "",
      body e
    ]

formatSenders :: [EmailAddress] -> Text
formatSenders = T.intercalate ", " . map fmt
  where
    fmt ea = case Fastmail.name ea of
      Just n -> n <> " <" <> Fastmail.email ea <> ">"
      Nothing -> Fastmail.email ea

schema :: Value
schema =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ "start" .= object ["type" .= ("integer" :: Text), "description" .= ("Pagination offset, default 0" :: Text)],
            "limit" .= object ["type" .= ("integer" :: Text), "description" .= ("Number of emails to fetch, default 20" :: Text)]
          ]
    ]
