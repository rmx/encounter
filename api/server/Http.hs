{-# LANGUAGE OverloadedStrings #-}

module Http
    (
    -- * Snap helpers
      parseRequestBody
    , textParam
    , optionalTextParam
    ) where


import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Monoid
import           Data.Int

import           Snap (readRequestBody, getParam)
import           Types



maxRequestBodySize :: Int64
maxRequestBodySize = 10 * 1000 * 1000


parseRequestBody :: (FromJSON a) => RequestHandler a
parseRequestBody = do
    body <- readRequestBody maxRequestBodySize
    case eitherDecode' body of
        Right x -> return x
        Left e  -> failWith $ BadRequest $
            "parseRequestBody: " <> (T.pack $ show e)


textParam :: ByteString -> RequestHandler Text
textParam name = do
    mbValue <- getParam name
    case mbValue of
        Just x  -> return $ T.decodeUtf8 x
        Nothing -> failWith $ BadRequest $
            "required param not present: " <> (T.decodeUtf8 name)


optionalTextParam :: ByteString -> RequestHandler (Maybe Text)
optionalTextParam name = do
    mbValue <- getParam name
    return $ fmap T.decodeUtf8 mbValue
