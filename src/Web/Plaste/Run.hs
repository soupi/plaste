{- | The entry point to the application.

It will configure the app by parsing the command line arguments
and will execute the app according to a command which can be:

- http
- https
- both

more information about those in the Web.Plaste.Config file which
is responsible about parsing arguments and configuration

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}  -- One more extension.
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}  -- To derive Show
{-# LANGUAGE TypeOperators      #-}


module Web.Plaste.Run where

import Web.Plaste.Types
import Web.Plaste.Database
import Web.Plaste.Router

import Data.ByteString.Char8 (pack)

import Web.Spock
import Web.Spock.Config


-- | This is the entry point of the application
--
run :: String -> Int -> IO ()
run connstr port = do
  let
    connStr
      | connstr == "default" =
        "host=localhost dbname=plaste port=5432 user=plaste password=plaste"
      | otherwise = connstr
  spockCfg <- (\cfg -> cfg { spc_csrfProtection = False })
    <$> defaultSpockCfg EmptySession (PCConn $ hasqlPool $ pack connStr) EmptyState

  runSpock port (spock spockCfg appRouter)
