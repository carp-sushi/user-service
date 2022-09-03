{-# LANGUAGE TypeFamilies #-}
module Web.Utils where

import Data.Aeson hiding (json)
import Data.Text  as T

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text.Encoding
import Database.Persist.Sql
import Network.HTTP.Types.Status
import Web.Spock

-- SQL query helper
{-# INLINE runSQL #-}
runSQL
  :: (HasSpock m, SpockConn m ~ SqlBackend)
  => SqlPersistT (LoggingT IO) a
  -> m a
runSQL action =
  runQuery $ \conn ->
    runStdoutLoggingT $ runSqlConn action conn

-- JSON error helper
{-# INLINE errorJson #-}
errorJson :: MonadIO m => Int -> Text -> ActionCtxT ctx m ()
errorJson code message =
  json $ object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]

-- Render all errors as JSON
jsonErrorHandler :: Status -> ActionCtxT () IO ()
jsonErrorHandler (Status code message) = errorJson code (decodeUtf8 message)

-- Parse the request body as json and fail with 400 status code on error.
-- This is a fix for a problem in spock where plain text is rendered in jsonBody'
-- when decoding JSON fails.
{-# INLINE jsonBody'' #-}
jsonBody'' :: (MonadIO m, FromJSON a) => ActionCtxT ctx m a
jsonBody'' = do
  b <- body
  case eitherDecodeStrict b of
    Right val -> return val
    Left err -> do
      setStatus status400
      let code = 400 :: Int
      json $ object
        [ "result" .= String "failure"
        , "error" .= object ["code" .= code, "message" .= err]
        ]
