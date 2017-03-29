{-# LANGUAGE OverloadedStrings #-}

module Routes (routes) where

import Data.Int (Int64)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
  ( ScottyM
  , get
  , post
  , delete
  , json
  , param
  )

import Models (getRecipe, getRecipes, addRecipe)

routes :: ScottyM ()
routes = do
  get "/recipes" $ do
    rs <- liftIO getRecipes
    json (rs)

  post "/recipes" $ do
    url <- param "url"
    now <- liftIO getCurrentTime
    r <- liftIO $ addRecipe url now
    json (r)

  get "/recipes/:id" $ do
    id <- param "id"
    maybeR <- liftIO $ getRecipe id
    case maybeR of
      Nothing -> json ("error: Id not found" :: String)
      Just r -> json (r)

  -- TODO: Finish below endpoints:
  delete "/recipes/:id" $ do
    id <- param "id"
    json (id :: String)

  get "/recipes/random" $ do
    json ("TODO" :: String)

