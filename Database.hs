my types.hs: {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data OkResp = OkResp
  { okRespValue :: Bool
  } deriving (Show, Generic)

instance ToJSON OkResp where
  toJSON (OkResp okv) = object ["ok" .= okv]

data ItemsResp a = ItemsResp
  { itemsRespValue :: [a]
  } deriving (Show, Generic)

instance ToJSON a => ToJSON (ItemsResp a) where
  toJSON (ItemsResp xs) = object ["items" .= xs]

data ContentResp = ContentResp
  { contentRespValue :: Text
  } deriving (Show, Generic)

instance ToJSON ContentResp where
  toJSON (ContentResp c) = object ["content" .= c]

data JobIdResp = JobIdResp
  { jobIdRespValue :: Text
  } deriving (Show, Generic)

instance ToJSON JobIdResp where
  toJSON (JobIdResp j) = object ["jobId" .= j]

data ImportFile = ImportFile
  { importFilePath :: Text
  , importFileContent :: Text
  } deriving (Show, Generic)

instance ToJSON ImportFile where
  toJSON (ImportFile p c) = object ["path" .= p, "content" .= c]

instance FromJSON ImportFile where
  parseJSON = withObject "ImportFile" $ \o ->
    ImportFile <$> o .: "path" <*> o .: "content"

data ImportResp
  = ImportOk [ImportFile]
  | ImportErr Text
  deriving (Show, Generic)

instance ToJSON ImportResp where
  toJSON (ImportOk fs) = object ["ok" .= True, "files" .= fs]
  toJSON (ImportErr e) = object ["ok" .= False, "error" .= e]

data WorkspaceBackup = WorkspaceBackup
  { backupWorkspace :: Text
  , backupFiles :: [ImportFile]
  } deriving (Show, Generic)

instance ToJSON WorkspaceBackup where
  toJSON (WorkspaceBackup w fs) = object
    [ "workspace" .= w
    , "files" .= fs
    ]

instance FromJSON WorkspaceBackup where
  parseJSON = withObject "WorkspaceBackup" $ \o ->
    WorkspaceBackup <$> o .: "workspace" <*> o .: "files"

data WorkspaceMeta = WorkspaceMeta
  { wmWorkspace       :: Text
  , wmUserId          :: Integer
  , wmCreatedAt       :: UTCTime
  , wmUpdatedAt       :: UTCTime
  , wmFileWrites      :: Integer
  , wmBuildCount      :: Integer
  , wmLastBuildStatus :: Maybe Text
  , wmLastBuildAt     :: Maybe UTCTime
  } deriving (Show, Generic)

instance ToJSON WorkspaceMeta where
  toJSON = genericToJSON defaultOptions

instance FromJSON WorkspaceMeta where
  parseJSON = genericParseJSON defaultOptions

data BuildRecord = BuildRecord
  { brJobId        :: Text
  , brWorkspace    :: Text
  , brUserId       :: Integer
  , brSelectedPath :: Text
  , brStartedAt    :: UTCTime
  , brFinishedAt   :: Maybe UTCTime
  , brOk           :: Maybe Bool
  } deriving (Show, Generic)

instance ToJSON BuildRecord where
  toJSON = genericToJSON defaultOptions

instance FromJSON BuildRecord where
  parseJSON = genericParseJSON defaultOptions my cardano-ide-api.cabal: cabal-version:      2.4
name:               cardano-ide-api
version:            0.1.0.0
build-type:         Simple

executable cardano-ide-api
  main-is:          Main.hs
  hs-source-dirs:   src
  default-language: GHC2021
  ghc-options:      -O2 -threaded -rtsopts -with-rtsopts=-N

  other-modules:
      Database
    , Jobs
    , SafePath
    , Schema
    , Types
    , Workspace
    , WorkspaceImport

  build-depends:
      base
    , aeson
    , bytestring
    , case-insensitive
    , containers
    , directory
    , filepath
    , http-types
    , mysql-simple
    , network
    , process
    , raw-strings-qq
    , random
    , text
    , time
    , transformers
    , unordered-containers
    , wai
    , wai-cors
    , wai-extra
    , warp my docker-compose.yml: services:
  mysql:
    image: mysql:8.4
    restart: unless-stopped
    environment:
      MYSQL_ROOT_PASSWORD: rootpassword
      MYSQL_DATABASE: cardano_ide
      MYSQL_USER: cardano_ide_user
      MYSQL_PASSWORD: cardano_ide_pass
    ports:
      - "3306:3306"
    volumes:
      - mysql-data:/var/lib/mysql
      - ./apps/haskell-api/db/init.sql:/docker-entrypoint-initdb.d/init.sql

  phpmyadmin:
    image: phpmyadmin:latest
    restart: unless-stopped
    environment:
      PMA_HOST: mysql
      PMA_PORT: 3306
      PMA_USER: root
      PMA_PASSWORD: rootpassword
    ports:
      - "8081:80"
    depends_on:
      - mysql

  haskell-api:
    build: ./apps/haskell-api
    environment:
      - WORKSPACE_ROOT=/data/workspaces
      - ALLOWED_ORIGINS=http://127.0.0.1:5173,http://localhost:5173,http://127.0.0.1:5174,http://localhost:5174,http://127.0.0.1:5175,http://localhost:5175
      - MYSQL_HOST=mysql
      - MYSQL_PORT=3306
      - MYSQL_DATABASE=cardano_ide
      - MYSQL_USER=cardano_ide_user
      - MYSQL_PASSWORD=cardano_ide_pass
    volumes:
      - ./data/workspaces:/data/workspaces
    ports: ["8080:8080"]
    depends_on:
      - mysql

  compiler-worker:
    build: ./apps/compiler-worker
    environment:
      - WORKSPACE_ROOT=/data/workspaces
      - DEFAULT_PROJECT=demo
      - COMPILE_CMD=bash -lc "cd /data/workspaces/demo && cabal build 2>&1"
    volumes:
      - ./data/workspaces:/data/workspaces

  frontend:
    image: node:20-alpine
    working_dir: /app
    volumes:
      - ./:/app
    ports: ["5173:5173"]
    environment:
      - NODE_OPTIONS=--dns-result-order=ipv4first
    command: sh -lc "npm i -g pnpm@9 && pnpm i && pnpm -C apps/shell-frontend dev --host 0.0.0.0"
    depends_on:
      - haskell-api

volumes:
  mysql-data:
