{-# LANGUAGE TypeFamilies #-}
module Web.Utils where

import Data.Aeson hiding (json)

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text
import Data.Text.Encoding
import Database.Persist.Sql
import Network.HTTP.Types.Status
import Web.Spock

-- SQL query helper
runSQL :: (HasSpock m, SpockConn m ~ SqlBackend) => SqlPersistT (LoggingT IO) a -> m a
runSQL action =
  runQuery $ \conn ->
    runStdoutLoggingT $ runSqlConn action conn
{-# INLINE runSQL #-}

-- JSON error helper
errorJson :: MonadIO m => Int -> Text -> ActionCtxT ctx m ()
errorJson code message =
  json $
    object
    [ "result" .= String "failure"
    , "error" .= object ["code" .= code, "message" .= message]
    ]
{-# INLINE errorJson #-}

-- Render uncaught errors as JSON
jsonErrorHandler :: Status -> ActionCtxT () IO ()
jsonErrorHandler (Status code message) = errorJson code (decodeUtf8 message)

