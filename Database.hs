{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database
  ( initDatabase
  , saveWorkspaceMeta
  , loadWorkspaceMeta
  , getWorkspaceMeta
  , appendBuildLog
  , loadBuildLogs
  , getBuildHistory
  , recordBuildStarted
  , recordBuildFinished
  , recordWorkspaceCreated
  , recordWorkspaceRenamed
  , recordWorkspaceDeleted
  , recordWorkspaceCleared
  , recordWorkspaceRestored
  , recordFileWrite
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Database.MySQL.Simple
  ( Connection
  , ConnectInfo(..)
  , Only(..)
  , close
  , connect
  , defaultConnectInfo
  , execute
  , execute_
  , query
  , query_
  )
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Types (BuildRecord(..), WorkspaceMeta(..))

mysqlConnectInfo :: IO ConnectInfo
mysqlConnectInfo = do
  host <- lookupEnv "MYSQL_HOST"
  portStr <- lookupEnv "MYSQL_PORT"
  db <- lookupEnv "MYSQL_DATABASE"
  user <- lookupEnv "MYSQL_USER"
  pass <- lookupEnv "MYSQL_PASSWORD"

  let mysqlHost = maybe "127.0.0.1" id host
  let mysqlPort = maybe 3306 id (portStr >>= readMaybe)
  let mysqlDb   = maybe "cardano_ide" id db
  let mysqlUser = maybe "cardano_ide_user" id user
  let mysqlPass = maybe "cardano_ide_pass" id pass

  pure defaultConnectInfo
    { connectHost = mysqlHost
    , connectPort = mysqlPort
    , connectDatabase = mysqlDb
    , connectUser = mysqlUser
    , connectPassword = mysqlPass
    }

withMySQL :: (Connection -> IO a) -> IO a
withMySQL action = do
  ci <- mysqlConnectInfo
  bracket (connect ci) close action

defaultUserId :: IO Integer
defaultUserId =
  withMySQL $ \conn -> do
    rows <-
      query_
        conn
        "SELECT id FROM users WHERE email = 'local@cardano-ide.dev' LIMIT 1"
        :: IO [Only Int64]

    case rows of
      (Only uid:_) -> pure (fromIntegral uid)
      [] -> do
        void $
          execute
            conn
            "INSERT INTO users (email, display_name, auth_provider, provider_user_id) VALUES (?, ?, ?, ?)"
            ("local@cardano-ide.dev" :: Text, "Local User" :: Text, "local" :: Text, "local-user" :: Text)

        rows2 <-
          query_
            conn
            "SELECT id FROM users WHERE email = 'local@cardano-ide.dev' LIMIT 1"
            :: IO [Only Int64]

        case rows2 of
          (Only uid:_) -> pure (fromIntegral uid)
          []           -> pure 1

ensureWorkspaceRow :: Connection -> Integer -> Text -> IO ()
ensureWorkspaceRow conn userId workspace = do
  void $
    execute
      conn
      "INSERT INTO workspaces (name, user_id) VALUES (?, ?) \
      \ON DUPLICATE KEY UPDATE updated_at = CURRENT_TIMESTAMP"
      (workspace, userId)

insertFileEvent :: Connection -> Integer -> Text -> Text -> Text -> IO ()
insertFileEvent conn userId workspace filePath eventType = do
  void $
    execute
      conn
      "INSERT INTO file_events (user_id, workspace_name, file_path, event_type) VALUES (?, ?, ?, ?)"
      (userId, workspace, filePath, eventType)

initDatabase :: IO ()
initDatabase =
  withMySQL $ \conn -> do
    void $
      execute_
        conn
        "CREATE TABLE IF NOT EXISTS users (\
        \  id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,\
        \  email VARCHAR(255) NOT NULL UNIQUE,\
        \  display_name VARCHAR(255) NULL,\
        \  auth_provider VARCHAR(64) NOT NULL DEFAULT 'local',\
        \  provider_user_id VARCHAR(255) NULL,\
        \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP\
        \)"

    void $
      execute_
        conn
        "CREATE TABLE IF NOT EXISTS workspaces (\
        \  name VARCHAR(255) NOT NULL PRIMARY KEY,\
        \  user_id BIGINT NOT NULL,\
        \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \  updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,\
        \  file_writes BIGINT NOT NULL DEFAULT 0,\
        \  build_count BIGINT NOT NULL DEFAULT 0,\
        \  last_build_status VARCHAR(32) NULL,\
        \  last_build_at TIMESTAMP NULL,\
        \  INDEX idx_workspaces_user_id (user_id),\
        \  CONSTRAINT fk_workspaces_user FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE\
        \)"

    void $
      execute_
        conn
        "CREATE TABLE IF NOT EXISTS builds (\
        \  job_id VARCHAR(255) NOT NULL PRIMARY KEY,\
        \  user_id BIGINT NOT NULL,\
        \  workspace_name VARCHAR(255) NOT NULL,\
        \  selected_path TEXT NOT NULL,\
        \  started_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \  finished_at TIMESTAMP NULL,\
        \  ok BOOLEAN NULL,\
        \  INDEX idx_builds_workspace_name (workspace_name),\
        \  INDEX idx_builds_user_id (user_id),\
        \  CONSTRAINT fk_builds_user FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE\
        \)"

    void $
      execute_
        conn
        "CREATE TABLE IF NOT EXISTS build_logs (\
        \  id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,\
        \  job_id VARCHAR(255) NOT NULL,\
        \  line_text TEXT NOT NULL,\
        \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \  INDEX idx_build_logs_job_id (job_id)\
        \)"

    void $
      execute_
        conn
        "CREATE TABLE IF NOT EXISTS file_events (\
        \  id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,\
        \  user_id BIGINT NOT NULL,\
        \  workspace_name VARCHAR(255) NOT NULL,\
        \  file_path TEXT NOT NULL,\
        \  event_type VARCHAR(64) NOT NULL,\
        \  created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,\
        \  INDEX idx_file_events_workspace_name (workspace_name),\
        \  INDEX idx_file_events_user_id (user_id),\
        \  CONSTRAINT fk_file_events_user FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE\
        \)"

    void $
      execute
        conn
        "INSERT IGNORE INTO users (email, display_name, auth_provider, provider_user_id) VALUES (?, ?, ?, ?)"
        ("local@cardano-ide.dev" :: Text, "Local User" :: Text, "local" :: Text, "local-user" :: Text)

saveWorkspaceMeta :: Text -> Value -> IO ()
saveWorkspaceMeta workspace value = do
  uid <- defaultUserId
  withMySQL $ \conn ->
    case Aeson.fromJSON value of
      Aeson.Success meta -> do
        void $
          execute
            conn
            "INSERT INTO workspaces \
            \(name, user_id, created_at, updated_at, file_writes, build_count, last_build_status, last_build_at) \
            \VALUES (?, ?, ?, ?, ?, ?, ?, ?) \
            \ON DUPLICATE KEY UPDATE \
            \  user_id = VALUES(user_id), \
            \  created_at = VALUES(created_at), \
            \  updated_at = VALUES(updated_at), \
            \  file_writes = VALUES(file_writes), \
            \  build_count = VALUES(build_count), \
            \  last_build_status = VALUES(last_build_status), \
            \  last_build_at = VALUES(last_build_at)"
            ( wmWorkspace meta
            , wmUserId meta
            , wmCreatedAt meta
            , wmUpdatedAt meta
            , wmFileWrites meta
            , wmBuildCount meta
            , wmLastBuildStatus meta
            , wmLastBuildAt meta
            )
      _ ->
        ensureWorkspaceRow conn uid workspace

loadWorkspaceMeta :: Text -> IO (Maybe WorkspaceMeta)
loadWorkspaceMeta workspace =
  withMySQL $ \conn -> do
    rows <-
      query
        conn
        "SELECT name, user_id, created_at, updated_at, file_writes, build_count, last_build_status, last_build_at \
        \FROM workspaces WHERE name = ? LIMIT 1"
        (Only workspace)
        :: IO [(Text, Int64, UTCTime, UTCTime, Int64, Int64, Maybe Text, Maybe UTCTime)]

    pure $ case listToMaybe rows of
      Just (ws, uid, createdAt, updatedAt, fileWrites, buildCount, lastBuildStatus, lastBuildAt) ->
        Just WorkspaceMeta
          { wmWorkspace = ws
          , wmUserId = fromIntegral uid
          , wmCreatedAt = createdAt
          , wmUpdatedAt = updatedAt
          , wmFileWrites = fromIntegral fileWrites
          , wmBuildCount = fromIntegral buildCount
          , wmLastBuildStatus = lastBuildStatus
          , wmLastBuildAt = lastBuildAt
          }
      Nothing -> Nothing

getWorkspaceMeta :: Text -> IO WorkspaceMeta
getWorkspaceMeta workspace = do
  meta <- loadWorkspaceMeta workspace
  case meta of
    Just m -> pure m
    Nothing -> do
      uid <- defaultUserId
      now <- getCurrentTime
      pure WorkspaceMeta
        { wmWorkspace = workspace
        , wmUserId = uid
        , wmCreatedAt = now
        , wmUpdatedAt = now
        , wmFileWrites = 0
        , wmBuildCount = 0
        , wmLastBuildStatus = Nothing
        , wmLastBuildAt = Nothing
        }

appendBuildLog :: Text -> Text -> IO ()
appendBuildLog jobId line =
  withMySQL $ \conn -> do
    void $
      execute
        conn
        "INSERT INTO build_logs (job_id, line_text) VALUES (?, ?)"
        (jobId, line)

loadBuildLogs :: Text -> IO [Text]
loadBuildLogs jobId =
  withMySQL $ \conn -> do
    rows <-
      query
        conn
        "SELECT line_text FROM build_logs WHERE job_id = ? ORDER BY id ASC"
        (Only jobId)
        :: IO [Only Text]
    pure [x | Only x <- rows]

getBuildHistory :: Text -> IO [BuildRecord]
getBuildHistory workspace =
  withMySQL $ \conn -> do
    rows <-
      query
        conn
        "SELECT job_id, workspace_name, user_id, selected_path, started_at, finished_at, ok \
        \FROM builds WHERE workspace_name = ? ORDER BY started_at DESC"
        (Only workspace)
        :: IO [(Text, Text, Int64, Text, UTCTime, Maybe UTCTime, Maybe Bool)]

    pure
      [ BuildRecord
          { brJobId = jobId
          , brWorkspace = ws
          , brUserId = fromIntegral uid
          , brSelectedPath = selectedPath
          , brStartedAt = startedAt
          , brFinishedAt = finishedAt
          , brOk = ok
          }
      | (jobId, ws, uid, selectedPath, startedAt, finishedAt, ok) <- rows
      ]

recordBuildStarted :: Text -> Text -> Text -> IO ()
recordBuildStarted project selectedPath jobId = do
  uid <- defaultUserId
  withMySQL $ \conn -> do
    ensureWorkspaceRow conn uid project

    void $
      execute
        conn
        "INSERT INTO builds (job_id, user_id, workspace_name, selected_path, started_at, finished_at, ok) \
        \VALUES (?, ?, ?, ?, CURRENT_TIMESTAMP, NULL, NULL) \
        \ON DUPLICATE KEY UPDATE \
        \  user_id = VALUES(user_id), \
        \  workspace_name = VALUES(workspace_name), \
        \  selected_path = VALUES(selected_path), \
        \  started_at = CURRENT_TIMESTAMP, \
        \  finished_at = NULL, \
        \  ok = NULL"
        (jobId, uid, project, selectedPath)

    void $
      execute
        conn
        "UPDATE workspaces \
        \SET build_count = build_count + 1, \
        \    last_build_status = ?, \
        \    last_build_at = CURRENT_TIMESTAMP, \
        \    updated_at = CURRENT_TIMESTAMP \
        \WHERE name = ?"
        (T.pack "started", project)

    insertFileEvent conn uid project selectedPath "build_started"

recordBuildFinished :: Text -> Text -> Bool -> IO ()
recordBuildFinished project jobId ok = do
  uid <- defaultUserId
  withMySQL $ \conn -> do
    ensureWorkspaceRow conn uid project

    void $
      execute
        conn
        "UPDATE builds \
        \SET finished_at = CURRENT_TIMESTAMP, ok = ? \
        \WHERE job_id = ?"
        (ok, jobId)

    void $
      execute
        conn
        "UPDATE workspaces \
        \SET last_build_status = ?, \
        \    last_build_at = CURRENT_TIMESTAMP, \
        \    updated_at = CURRENT_TIMESTAMP \
        \WHERE name = ?"
        (if ok then T.pack "success" else T.pack "failed", project)

    insertFileEvent conn uid project jobId (if ok then "build_success" else "build_failed")

recordWorkspaceCreated :: Text -> IO ()
recordWorkspaceCreated workspace = do
  uid <- defaultUserId
  withMySQL $ \conn -> do
    ensureWorkspaceRow conn uid workspace
    insertFileEvent conn uid workspace "" "workspace_created"

recordWorkspaceRenamed :: Text -> Text -> IO ()
recordWorkspaceRenamed oldName newName = do
  uid <- defaultUserId
  withMySQL $ \conn -> do
    _ :: Int64 <-
      execute
        conn
        "UPDATE workspaces SET name = ?, updated_at = CURRENT_TIMESTAMP WHERE name = ?"
        (newName, oldName)

    ensureWorkspaceRow conn uid newName

    void $
      execute
        conn
        "UPDATE builds SET workspace_name = ? WHERE workspace_name = ?"
        (newName, oldName)

    void $
      execute
        conn
        "UPDATE file_events SET workspace_name = ? WHERE workspace_name = ?"
        (newName, oldName)

    insertFileEvent conn uid newName oldName "workspace_renamed"

recordWorkspaceDeleted :: Text -> IO ()
recordWorkspaceDeleted workspace = do
  uid <- defaultUserId
  withMySQL $ \conn -> do
    ensureWorkspaceRow conn uid workspace
    insertFileEvent conn uid workspace "" "workspace_deleted"

recordWorkspaceCleared :: Text -> IO ()
recordWorkspaceCleared workspace = do
  uid <- defaultUserId
  withMySQL $ \conn -> do
    ensureWorkspaceRow conn uid workspace
    insertFileEvent conn uid workspace "" "workspace_cleared"

recordWorkspaceRestored :: Text -> IO ()
recordWorkspaceRestored workspace = do
  uid <- defaultUserId
  withMySQL $ \conn -> do
    ensureWorkspaceRow conn uid workspace
    insertFileEvent conn uid workspace "" "workspace_restored"

recordFileWrite :: Text -> Text -> IO ()
recordFileWrite workspace path = do
  uid <- defaultUserId
  withMySQL $ \conn -> do
    ensureWorkspaceRow conn uid workspace

    void $
      execute
        conn
        "UPDATE workspaces \
        \SET file_writes = file_writes + 1, updated_at = CURRENT_TIMESTAMP \
        \WHERE name = ?"
        (Only workspace)

    insertFileEvent conn uid workspace path "write"
