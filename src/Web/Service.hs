{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}
module Web.Service
  ( runService
  ) where

import Control.Monad
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
type ApiAction a = SpockAction SqlBackend () () a

-- Run the database migrations and server the REST api on the given port.
runService :: Config -> IO ()
runService cfg = do
  pool <- runStdoutLoggingT $ createSqlitePool (database cfg) (connections cfg)
  spockCfg' <- defaultSpockCfg () (PCPool pool) ()
  let spockCfg = spockCfg' {spc_errorHandler = jsonErrorHandler}
  runStdoutLoggingT $ runSqlPool (do runMigration migrateAll) pool
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

-- Health check action
getStatus = do
  json $ object ["status" .= String "ok"]

-- Query all users and render the list as JSON
getUsers = do
  users <- runSQL $ P.selectList [] [Asc UserId]
  json users

-- Query a user by ID and render as JSON
getUser userId = do
  maybeUser <- runSQL $ P.get userId :: ApiAction (Maybe User)
  case maybeUser of
    Just user -> json user
    Nothing -> do
      setStatus notFound404
      errorJson 404 "User not found"

-- Create a new user and return the new user ID
createUser = do
  user <- jsonBody'' :: ApiAction User
  userId <- runSQL $ P.insert user
  setStatus created201
  json $ object ["result" .= String "success", "id" .= userId]

-- Update an existing user and return the user ID
updateUser userId = do
  user <- jsonBody'' :: ApiAction User
  runSQL $ P.replace userId user
  setStatus created201
  json $ object ["result" .= String "success", "id" .= userId]

-- Delete an existing user and send an empty response
deleteUser userId = do
  userExists <- runSQL $ P.exists [UserId ==. userId]
  setStatus noContent204
  guard userExists
  runSQL $ P.delete (userId :: UserId)

