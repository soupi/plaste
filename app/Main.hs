{-# LANGUAGE LambdaCase #-}

module Main where

import System.Exit
import Text.Read
import System.Environment
import Web.Plaste

main :: IO ()
main =

  -- Run a command or get the serve mode
  getArgs >>= \case
    [connstr, portStr]
      | Just port <- readMaybe portStr -> do
        run connstr port

    _ -> do
      putStrLn "usage: plaste <connection-string> <port>"
      exitFailure
