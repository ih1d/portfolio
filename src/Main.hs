{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant (serve)

import Api (portfolioAPI, server)

main :: IO ()
main = do
  let port = 8080
      staticDir = "static"
  putStrLn $ "Starting server on http://localhost:" ++ show port
  run port $ serve portfolioAPI (server staticDir)
