{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import Routes (routes)
import Models (initDb)

main :: IO ()
main = do
  initDb
  scotty 3000 $ do
    routes

