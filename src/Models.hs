{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Int (Int64)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Control.Applicative
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
  ( mkMigrate
  , mkPersist
  , persistLowerCase
  , share
  , sqlSettings
  )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Recipe json
     url String
     date UTCTime
     deriving Show
|]


initDb :: IO ()
initDb = runDb $ runMigrationUnsafe migrateAll

-- TODO: Dont hardcode the db config:
runDb :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDb query =
  runNoLoggingT . runResourceT . withSqliteConn "recipedev.sqlite3" . runSqlConn $ query

getRecipe :: Int64 -> IO (Maybe Recipe)
getRecipe id = runDb $ get $ toSqlKey id

getRecipes :: IO [Entity Recipe]
getRecipes = runDb $ selectList [] [LimitTo 50]

addRecipe :: String -> UTCTime -> IO (Key Recipe)
addRecipe url now = runDb $ insert $ Recipe url now

