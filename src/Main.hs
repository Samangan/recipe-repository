{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Control.Monad.IO.Class (liftIO)

import Routes (routes)
import Models (initDb)

main :: IO ()
main = do
  initDb
  scotty 3000 $ do
    routes

