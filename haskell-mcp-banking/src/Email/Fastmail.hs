{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Email.Fastmail
  ( Session (..),
    Email (..),
    EmailAddress (..),
    Page,
    getMethodResponses,
    getSession,
    getMailboxIds,
    retrieveMailbox,
    getEmails,
    archiveEmail,
  )
where

import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except
import Data.Aeson (FromJSON (parseJSON), Value (..), object, withArray, withObject, (.:), (.=))
import Data.Aeson.Key as K
import Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Generics
import Network.Wreq (asJSON, defaults, getWith, postWith)
import Network.Wreq.Lens

-- Get Session API

data Session = Session
  { downloadUrl :: String,
    apiUrl :: String,
    accountId :: String
  }
  deriving (Show, Generic)

instance FromJSON Session where
  parseJSON = withObject "Session" $ \v ->
    Session
      <$> v .: "downloadUrl"
      <*> v .: "apiUrl"
      <*> (v .: "primaryAccounts" >>= (.: "urn:ietf:params:jmap:core"))

withBearerToken :: BS.ByteString -> Options -> Options
withBearerToken apiToken opts = opts & header "Authorization" .~ ["Bearer " <> apiToken]

getSession :: BS.ByteString -> ExceptT String IO Session
getSession apiToken = do
  let opts = withBearerToken apiToken defaults
  let url = "https://api.fastmail.com/jmap/session"
  r <- liftIO $ (try (getWith opts url >>= asJSON) :: IO (Either SomeException (Response Session)))

  case r of
    Left err -> throwE (show err)
    Right body -> return (body ^. responseBody)

-- JMAP Method Responses Data Type

data MethodResponses = MethodResponses
  { methodResponses :: [MethodResponse],
    sessionState :: String
  }
  deriving (Generic, FromJSON)

data MethodResponse
  = MailboxQueryResponse [String]
  | EmailQueryResponse [String]
  | EmailGetResponse [Email]
  | EmailSetResponse
  | ErrorResponse String

data EmailAddress = EmailAddress
  { name :: Maybe T.Text,
    email :: T.Text
  }
  deriving (Show, Generic, FromJSON)

data Email = Email
  { emailId :: String,
    from :: [EmailAddress],
    subject :: T.Text,
    body :: T.Text
  }
  deriving (Show)

data TextBody = TextBody
  { partId :: String,
    bodyType :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON TextBody where
  parseJSON = withObject "TextBody" $ \v ->
    TextBody
      <$> v .: "partId"
      <*> v .: "type"

data BodyValue = BodyValue
  { value :: T.Text
  }
  deriving (Generic, FromJSON)

instance FromJSON Email where
  parseJSON = withObject "Email" $ \v -> do
    eid <- v .: "id"
    eFrom <- v .: "from"
    subj <- v .: "subject"
    bParts <- v .: "textBody"
    bValues <- v .: "bodyValues"
    bodyText <- parseBody eid bParts bValues
    return $ Email eid eFrom subj bodyText
    where
      parseBody eid (TextBody pid t : rest) vs
        | t `elem` ["text/plain", "text/html"] = do
            let key = K.fromString pid
            case KM.lookup key vs of
              Just (BodyValue val) -> do
                next <- parseBody eid rest vs
                return $ val <> next
              Nothing -> fail $ "Missing body part " ++ pid ++ " for email " ++ eid
      parseBody eid (_ : rest) vs = parseBody eid rest vs
      parseBody _ [] _ = return ""

instance FromJSON MethodResponse where
  parseJSON = withArray "MethodResponse" (parseMethodResponse . V.toList)
    where
      parseMethodResponse [String "Mailbox/query", resp, _] =
        parseJSON resp >>= \obj -> MailboxQueryResponse <$> obj .: "ids"
      parseMethodResponse [String "Email/query", resp, _] =
        parseJSON resp >>= \obj -> EmailQueryResponse <$> obj .: "ids"
      parseMethodResponse [String "Email/get", resp, _] =
        parseJSON resp >>= \obj -> EmailGetResponse <$> obj .: "list"
      parseMethodResponse [String "Email/set", _, _] = return EmailSetResponse
      parseMethodResponse [String "error", err, _] =
        parseJSON err >>= \obj -> ErrorResponse <$> obj .: "type"
      parseMethodResponse _ = fail "Unhandled response type or invalid length"

-- Method Call

getMethodResponses :: Session -> BS.ByteString -> Value -> ExceptT String IO MethodResponses
getMethodResponses (Session {apiUrl}) apiToken req = do
  let opts = withBearerToken apiToken defaults

  r <-
    liftIO $
      (try (postWith opts apiUrl req >>= asJSON) :: IO (Either SomeException (Response MethodResponses)))

  case r of
    Left err -> throwE (show err)
    Right body -> return (body ^. responseBody)

-- Get Mailbox Id

mailboxQuery :: String -> BS.ByteString -> Value
mailboxQuery accountId mailboxName =
  object
    [ "using" .= ["urn:ietf:params:jmap:core" :: String, "urn:ietf:params:jmap:mail" :: String],
      "methodCalls"
        .= [ [ "Mailbox/query",
               object
                 [ "accountId" .= accountId,
                   "filter" .= object ["name" .= TE.decodeUtf8 mailboxName]
                 ],
               "mailboxQuery1"
             ]
           ]
    ]

getMailboxIds :: Session -> BS.ByteString -> BS.ByteString -> ExceptT String IO [String]
getMailboxIds session apiToken mailboxName = do
  let req = mailboxQuery (accountId session) mailboxName
  res <- getMethodResponses session apiToken req
  let responses = methodResponses res
  case [ids | MailboxQueryResponse ids <- responses] of
    (ids : _) -> return ids
    [] -> case [err | ErrorResponse err <- responses] of
      (err : _) -> throwE $ "JMAP Error: " ++ err
      [] -> throwE "JMAP Error: No mailbox query response found"

-- Email Query
type Page = (Integer, Integer)

emailQuery :: Page -> String -> String -> Value
emailQuery (start, limit) accountId mailboxId =
  object
    [ "using" .= ["urn:ietf:params:jmap:core" :: String, "urn:ietf:params:jmap:mail" :: String],
      "methodCalls"
        .= [ [ "Email/query",
               object
                 [ "accountId" .= accountId,
                   "filter" .= object ["inMailbox" .= mailboxId],
                   "position" .= start,
                   "limit" .= limit
                 ],
               "emailQuery1"
             ]
           ]
    ]

retrieveMailbox :: Page -> Session -> BS.ByteString -> String -> ExceptT String IO [String]
retrieveMailbox page session apiToken mailboxId = do
  let req = emailQuery page (accountId session) mailboxId
  res <- getMethodResponses session apiToken req
  let responses = methodResponses res
  case [ids | EmailQueryResponse ids <- responses] of
    (ids : _) -> return ids
    [] -> case [err | ErrorResponse err <- responses] of
      (err : _) -> throwE $ "JMAP Error: " ++ err
      [] -> throwE "JMAP Error: No email query response found"

-- Get Email

emailGet :: String -> [String] -> Value
emailGet accountId ids =
  object
    [ "using" .= ["urn:ietf:params:jmap:core" :: String, "urn:ietf:params:jmap:mail" :: String],
      "methodCalls"
        .= [ [ "Email/get",
               object
                 [ "accountId" .= accountId,
                   "ids" .= ids,
                   "properties" .= (["id", "from", "subject", "textBody", "bodyValues"] :: [T.Text]),
                   "fetchTextBodyValues" .= True
                 ],
               "emailGet1"
             ]
           ]
    ]

getEmails :: Session -> BS.ByteString -> [String] -> ExceptT String IO [Email]
getEmails session apiToken ids = do
  let req = emailGet (accountId session) ids
  res <- getMethodResponses session apiToken req
  let responses = methodResponses res
  case [emails | EmailGetResponse emails <- responses] of
    (emails : _) -> return emails
    [] -> case [err | ErrorResponse err <- responses] of
      (err : _) -> throwE $ "JMAP Error: " ++ err
      [] -> throwE "JMAP Error: No email get response found"

-- Archive Emails

emailArchive :: String -> String -> [String] -> Value
emailArchive accountId archiveId ids =
  object
    [ "using" .= ["urn:ietf:params:jmap:core" :: String, "urn:ietf:params:jmap:mail" :: String],
      "methodCalls"
        .= [ [ "Email/set",
               object
                 [ "accountId" .= accountId,
                   "update"
                     .= object
                       [ K.fromString eid .= object ["mailboxIds" .= object [K.fromString archiveId .= True]]
                       | eid <- ids
                       ]
                 ],
               "archive1"
             ]
           ]
    ]

archiveEmail :: Session -> BS.ByteString -> String -> [String] -> ExceptT String IO ()
archiveEmail session apiToken archiveId ids = do
  let eArchive = emailArchive (accountId session) archiveId ids
  res <- getMethodResponses session apiToken eArchive
  let responses = methodResponses res
  case [() | EmailSetResponse <- responses] of
    (_ : _) -> return ()
    [] -> case [err | ErrorResponse err <- responses] of
      (err : _) -> throwE $ "JMAP Error: " ++ err
      [] -> throwE "JMAP Error: No email set response found"
