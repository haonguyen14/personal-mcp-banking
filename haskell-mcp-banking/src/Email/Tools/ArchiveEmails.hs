{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Email.Tools.ArchiveEmails (ArchiveEmailsTool (..)) where

import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Aeson (FromJSON (..), Value, object, withObject, (.:), (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Email.Fastmail
import Tool

data ArchiveEmailsTool = ArchiveEmailsTool BS.ByteString

newtype ArchiveEmailsInput = ArchiveEmailsInput
  { emailIds :: [String]
  }

instance FromJSON ArchiveEmailsInput where
  parseJSON = withObject "ArchiveEmailsInput" $ \o ->
    ArchiveEmailsInput <$> o .: "emailIds"

instance ToolCapability ArchiveEmailsTool where
  type Input ArchiveEmailsTool = ArchiveEmailsInput

  getMetadata _ =
    ToolMetadata
      { name = "archive_emails",
        title = Just "Archive Emails",
        description = Just "Move emails to the Archive mailbox by their IDs. Use get_emails to find email IDs.",
        inputSchema = schema
      }

  runTool (ArchiveEmailsTool apiToken) input = do
    result <- runExceptT $ do
      session <- getSession apiToken
      archiveIds <- getMailboxIds session apiToken "Archive"
      case archiveIds of
        [] -> throwE "Archive mailbox not found"
        (archiveId : _) -> archiveEmail session apiToken archiveId (emailIds input)
    case result of
      Left err -> return $ ToolResult [ToolText $ T.pack err] True
      Right () ->
        return $
          ToolResult
            [ToolText $ "Archived " <> T.pack (show (length (emailIds input))) <> " email(s)."]
            False

schema :: Value
schema =
  object
    [ "type" .= ("object" :: Text),
      "required" .= (["emailIds"] :: [Text]),
      "properties"
        .= object
          [ "emailIds"
              .= object
                [ "type" .= ("array" :: Text),
                  "items" .= object ["type" .= ("string" :: Text)],
                  "description" .= ("List of email IDs to archive" :: Text)
                ]
          ]
    ]
