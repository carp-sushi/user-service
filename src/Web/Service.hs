{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Web.Service
  ( runService
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Network.HTTP.Types.Status
import Web.Spock
import Web.Spock.Config

import           Data.Aeson              hiding (json)
import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (delete, get)

import Web.Config
import Web.Models
import Web.Utils

-- The API type
type Api = SpockM SqlBackend () () ()

-- The API action type
type Action a = SpockAction SqlBackend () () a

-- Make type sigs more readable
type WebStateSql = WebStateM SqlBackend () ()

-- Run database migrations then start the web service.
runService :: Config -> IO ()
runService cfg = do
  pool <- runStdoutLoggingT $ createSqlitePool (database cfg) (connections cfg)
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  spockCfg' <- defaultSpockCfg () (PCPool pool) ()
  let spockCfg = spockCfg' {spc_errorHandler = jsonErrorHandler}
  runSpock (port cfg) (spock spockCfg api)

-- The core spock API
api :: Api
api = do
  get "status" getStatus
  get "users" getUsers
  get ("users" <//> var) getUser
  post "users" createUser
  put ("users" <//> var) updateUser
  delete ("users" <//> var) deleteUser

-- Health check
getStatus :: MonadIO m => ActionCtxT () m a
getStatus = do
  json $ object ["status" .= String "ok"]

-- Query all users and render the list as JSON
getUsers :: ActionCtxT () WebStateSql a
getUsers = do
  users <- runSQL $ P.selectList [] [Asc UserId]
  json users

-- Query a user by ID and render as JSON
getUser :: UserId -> ActionCtxT () WebStateSql a
getUser userId = do
  maybeUser <- runSQL $ P.get userId :: Action (Maybe User)
  case maybeUser of
    Just user -> json user
    Nothing -> do
      setStatus notFound404
      json $ object
        ["result" .= String "error", "error" .= String "User not found"]

-- Create a new user and return the new user ID
createUser :: ActionCtxT () WebStateSql a
createUser = do
  user <- decodeJsonBody :: Action User
  userId <- runSQL $ P.insert user
  setStatus created201
  json $ object ["result" .= String "success", "userId" .= userId]

-- Update an existing user and return the user ID
updateUser :: UserId -> ActionCtxT () WebStateSql a
updateUser userId = do
  user <- decodeJsonBody :: Action User
  runSQL $ P.replace userId user
  setStatus created201
  json $ object ["result" .= String "success", "userId" .= userId]

-- Delete an existing user and send an empty response
deleteUser :: UserId -> ActionCtxT () WebStateSql ()
deleteUser userId = do
  userExists <- runSQL $ P.exists [UserId ==. userId]
  setStatus noContent204
  guard userExists
  runSQL $ P.delete (userId :: UserId)

