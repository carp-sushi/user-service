{-# LANGUAGE TypeFamilies #-}

module Web.Utils (
  jsonBody,
  jsonErrorHandler,
  runSQL,
) where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson hiding (json)
import Data.Text.Encoding
import Database.Persist.Sql
import Network.HTTP.Types.Status
import Web.Spock hiding (jsonBody)

-- SQL query helper
{-# INLINE runSQL #-}
runSQL ::
  (HasSpock m, SpockConn m ~ SqlBackend) =>
  SqlPersistT (NoLoggingT IO) a ->
  m a
runSQL action =
  runQuery $ \conn ->
    runNoLoggingT $ runSqlConn action conn

-- Render uncaught errors as JSON
jsonErrorHandler :: Status -> ActionCtxT () IO ()
jsonErrorHandler (Status _ message) =
  json $ object ["result" .= String "error", "error" .= decodeUtf8 message]

-- Parse the request body as json and fail with 400 status code on error.
-- This is a fix for a problem in spock where plain text is rendered in
-- jsonBody' when decoding JSON fails.
{-# INLINE jsonBody #-}
jsonBody :: (MonadIO m, FromJSON a) => ActionCtxT ctx m a
jsonBody = do
  b <- body
  case eitherDecodeStrict b of
    Right val -> return val
    Left err -> do
      setStatus status400
      json $ object ["result" .= String "error", "error" .= err]
