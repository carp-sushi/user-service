{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Web.Service
  ( runService
  , service
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import           Data.Aeson      hiding (json)
import           Data.Text
import qualified Data.Validation as V

import qualified Database.Persist        as P
import           Database.Persist.Sqlite hiding (delete, get)

import Network.HTTP.Types.Status
import Network.Wai

import Web.Spock        hiding (jsonBody)
import Web.Spock.Config

import Web.Config
import Web.Models
import Web.Utils
import Web.Validate

-- The API type
type Api = SpockM SqlBackend () () ()

-- The API action type
type Action a = SpockAction SqlBackend () () a

-- Make type sigs a bit more readable
type ApiWebState = WebStateM SqlBackend () ()
type ActionCtx a = ActionCtxT () ApiWebState a

-- Run database migrations then start the web service.
runService :: Config -> IO ()
runService cfg = do
  let db = database cfg
      conns = connections cfg
  runSpock (port cfg) (service db conns)

-- Web service definition
service :: Text -> Int -> IO Middleware
service dbname conns = do
  pool <- runNoLoggingT $ createSqlitePool dbname conns
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
  spockCfg' <- defaultSpockCfg () (PCPool pool) ()
  let spockCfg = spockCfg' {
    spc_errorHandler = jsonErrorHandler,
    spc_maxRequestSize = Just (1024 * 1024) -- 1MB
  }
  spock spockCfg api

-- The core spock API for the user service.
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
getUsers :: ActionCtx a
getUsers = do
  users <- runSQL $ P.selectList [] [Asc UserId]
  json users

-- Query a user by ID and render as JSON
getUser :: UserId -> ActionCtx a
getUser userId = do
  maybeUser <- runSQL $ P.get userId :: Action (Maybe User)
  case maybeUser of
    Nothing -> do
      setStatus notFound404
      json $ object
        ["result" .= String "error", "error" .= String "User not found"]
    Just user ->
      json user

-- Create a new user and return the new user ID
createUser :: ActionCtx a
createUser = do
  req <- jsonBody :: Action User
  case validateUser req of
    V.Success user -> do
      userId <- runSQL $ P.insert user
      setStatus created201
      json $ object ["result" .= String "success", "userId" .= userId]
    V.Failure e -> do
      setStatus badRequest400
      json $ object ["result" .= String "error", "error" .= errorMessage e]

-- Update an existing user and return the user ID
updateUser :: UserId -> ActionCtx a
updateUser userId = do
  req <- jsonBody :: Action User
  case validateUser req of
    V.Success user -> do
      runSQL $ P.replace userId user
      setStatus created201
      json $ object ["result" .= String "success", "userId" .= userId]
    V.Failure e -> do
      setStatus badRequest400
      json $ object ["result" .= String "error", "error" .= errorMessage e]

-- Delete an existing user and send an empty response
deleteUser :: UserId -> ActionCtx ()
deleteUser userId = do
  userExists <- runSQL $ P.exists [UserId ==. userId]
  setStatus noContent204
  guard userExists
  runSQL $ P.delete (userId :: UserId)

