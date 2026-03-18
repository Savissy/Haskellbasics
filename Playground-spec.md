Imitate Word Press
-----------------
-- AI prompt 
-- it must use cardano wallet(Coxy Wallet) on preprod testnet
1. languages - Haskell/aiken/opshin/helios...
2. link to github
(1) Examples - a) Lock/Unlock b) ...
    i)      - onchain
        - unit tests
    ii)     - offchain
        - unit tests
    iii)     - front end
        - unit tests
    iv)    - backend
        - database
        - server side 
        - tests
    v)     - different very nice templates for frontend
(2) Editor
(3) Wizard style building
3. IDE - Compile - Test - Deploy





Plutus Playground Studio

    - Frontend
    - Server Side API
    - Database 
    - file system
    
1. languages - Haskell/aiken/opshin/helios.../midnight

2. Examples - a) Lock/Unlock b) market place c) etc
    
3. For Each example this is displayed
    i)      - onchain
        - unit tests
    ii)     - offchain
        - unit tests
    iii)     - front end
        - unit tests
    iv)    - backend
        - database
        - server side 
        - tests
    v)     - different very nice templates for frontend
    vi)     - also link to hosted application
4.     a)   Wizard style building script
        i)   Compile
        ii)  Test
        iii) Deploy 
    b) Editor
        i)     code
        ii)    debug
        i)     Compile
        ii)    Test
        iii)   Deploy 


Imitate Word Press Easy of Use
------------------------------

    -- AI integrated
    -- Coxy Wallet integrated


1351.6 Building executable 'cardano-ide-api' for cardano-ide-api-0.1.0.0..
1351.9 [1 of 8] Compiling SafePath         ( src/SafePath.hs, /app/dist-newstyle/build/x86_64-linux/ghc-9.6.4/cardano-ide-api-0.1.0.0/x/cardano-ide-api/build/cardano-ide-api/cardano-ide-api-tmp/SafePath.o )
1352.2 [2 of 8] Compiling Schema           ( src/Schema.hs, /app/dist-newstyle/build/x86_64-linux/ghc-9.6.4/cardano-ide-api-0.1.0.0/x/cardano-ide-api/build/cardano-ide-api/cardano-ide-api-tmp/Schema.o )
1352.5 [3 of 8] Compiling Types            ( src/Types.hs, /app/dist-newstyle/build/x86_64-linux/ghc-9.6.4/cardano-ide-api-0.1.0.0/x/cardano-ide-api/build/cardano-ide-api/cardano-ide-api-tmp/Types.o )
1359.6 [4 of 8] Compiling Database         ( src/Database.hs, /app/dist-newstyle/build/x86_64-linux/ghc-9.6.4/cardano-ide-api-0.1.0.0/x/cardano-ide-api/build/cardano-ide-api/cardano-ide-api-tmp/Database.o )
1359.6 
1359.6 src/Database.hs:32:1: error:
1359.6     Could not find module ‘Database.MySQL.Simple.FromRow’
1359.6     Perhaps you meant
1359.6       Database.MySQL.Simple.Param (from mysql-simple-0.4.9)
1359.6       Database.MySQL.Simple.Types (from mysql-simple-0.4.9)
1359.6       Database.MySQL.Simple.Result (from mysql-simple-0.4.9)
1359.6     Use -v (or `:set -v` in ghci) to see a list of the files searched for.
1359.6    |
1359.6 32 | import Database.MySQL.Simple.FromRow
1359.6    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
1359.6 [6 of 8] Compiling Workspace        ( src/Workspace.hs, /app/dist-newstyle/build/x86_64-linux/ghc-9.6.4/cardano-ide-api-0.1.0.0/x/cardano-ide-api/build/cardano-ide-api/cardano-ide-api-tmp/Workspace.o )
1361.6 [7 of 8] Compiling WorkspaceImport  ( src/WorkspaceImport.hs, /app/dist-newstyle/build/x86_64-linux/ghc-9.6.4/cardano-ide-api-0.1.0.0/x/cardano-ide-api/build/cardano-ide-api/cardano-ide-api-tmp/WorkspaceImport.o )
1361.6 Error: cabal: Failed to build exe:cardano-ide-api from
1361.6 cardano-ide-api-0.1.0.0.
1361.6 
------
[+] build 0/1
 ⠙ Image ide-haskell-api Building                                                                                                  1470.6s
Dockerfile:29

--------------------

  27 |     COPY db /app/db

  28 |     

  29 | >>> RUN cabal update && cabal build -j

  30 |     

  31 |     EXPOSE 8080

--------------------

failed to solve: process "/bin/sh -c cabal update && cabal build -j" did not complete successfully: exit code: 1

[+] up 25/36
 ⠴ Image phpmyadmin:latest [⣿⣿⣿⣿⣿⣿⠀⣿⣿⣀⣿⣦⣿⣿⣿⣿⣿⣦⣿⣿⣤⣿] 54.51MB / 196.5MB Pulling                                                       540.6s
 ⠦ Image mysql:8.4 [⣿⣿⣿⠀⣿⡀⣿⣿⣿⡀⣿⣄] 42.59MB / 248.5MB                   Pulling                                                       540.6s
short read: expected 15234346 bytes but got 6701056: unexpected EOF
dell_8th_gen_core_i5@DESKTOP-R4MNH88:~/haskel-IDE/IDE$ 


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
import Data.Time.Clock (getCurrentTime)
import Database.MySQL.Simple
  ( Connection
  , ConnectInfo(..)
  , Only(..)
  , close
  , connect
  , defaultConnectInfo
  , execute
  , execute_
  , field
  , fromRow
  , query
  , query_
  )
import Database.MySQL.Simple.QueryParams ()
import Database.MySQL.Simple.Result ()
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import Types (BuildRecord(..), WorkspaceMeta(..))
