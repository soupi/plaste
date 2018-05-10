{- | The router of the application

The router directs http requests to the relevant handler/action to be taken

-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.Plaste.Router where

import Web.Plaste.Html
import Web.Plaste.Types
import Web.Plaste.Database

import Data.Int (Int64)
import qualified Data.ByteString as BS
import Data.Text (pack, Text)
import qualified Data.Text as T
import Data.Text.IO as T
import Data.Text.Encoding (decodeUtf8)

import Control.Monad.IO.Class
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.HTTP.Types
import Control.Exception
import Web.Spock
import Web.Spock.Lucid


-------------
-- Routing --
-------------

-- | This is the router of the app
--
appRouter :: App ()
appRouter = do

  -- serve static content like css and js that can be found in the static folder

  middleware (staticPolicy (addBase "static"))

  -- display events

  get ("raw" <//> var) $ \(plasteId :: Int64) -> do
    setHeader "Access-Control-Allow-Origin" "*"
    readQuery (getPlasteById plasteId) >>= \case
      Left err -> do
        setStatus status500
        text (pack $ show err)
      Right Nothing -> do
        setStatus status404
        text "Not found."
      Right (Just txt) ->
        text txt

  get var $ \(plasteId :: Int64) -> do
    setHeader "Access-Control-Allow-Origin" "*"
    readQuery (getPlasteById plasteId) >>= \case
      Left err -> do
        setStatus status500
        text (pack $ show err)
      Right Nothing -> do
        setStatus status404
        text "Not found."
      Right (Just txt) ->
        lucid $ showCode plasteId txt

  post var $ \(token :: Text) -> do
    setHeader "Access-Control-Allow-Origin" "*"
    tokens <- liftIO $ fmap T.lines (T.readFile "tokens.txt") `catch` \(SomeException _) -> pure []
    if token `elem` fmap (T.dropWhile (== ' ') . T.dropWhile (/= ' ')) tokens
      then do
        bs <- body
        if BS.length bs > limit
          then do
            setStatus status403
            text $ T.pack $ unlines
              [ "Paste length exceeded character limit."
              , "Limit: " ++ show limit
              , "Got:   " ++ show (BS.length bs)
              ]
          else do
            writeQuery (newPlaste $ decodeUtf8 bs) >>= \case
              Left err -> do
                setStatus status500
                text (pack $ show err)
              Right (Left err) -> do
                setStatus status500
                text (pack $ show err)
              Right (Right pid) -> do
                text (pack $ show pid)
      else do
        setStatus status401
        text "Invalid token."

limit :: Int
limit = 1000 * 80

