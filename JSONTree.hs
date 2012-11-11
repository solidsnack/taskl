{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
  #-}
module JSONTree where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Tree (Tree(..), Forest)

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector


instance FromJSON (Tree Text) where
  parseJSON (String s)   = return $ Node s []
  parseJSON (Bool False) = return $ Node "false" []
  parseJSON (Bool True)  = return $ Node "true" []
  parseJSON Null         = return $ Node "null" []
  parseJSON (Number n)   = return $ Node (Text.pack $ show n) []
  parseJSON a@(Array _)  = Node "" <$> parseJSON a
  parseJSON (Object o) | [(k, json)] <- HashMap.toList o = tree k json
                       | otherwise                       = mzero

instance FromJSON (Forest Text) where
  parseJSON (Array v)  = parseJSON `mapM` Vector.toList v
  parseJSON (Object o) = uncurry tree `mapM` HashMap.toList o
  parseJSON _          = mzero

tree :: Text -> Value -> Parser (Tree Text)
tree k json = Node k <$> case json of Array  _ -> parseJSON json
                                      Object _ -> parseJSON json
                                      _        -> (:[]) <$> parseJSON json

instance FromJSON (Tree ByteString) where
  parseJSON = ((Text.encodeUtf8 <$>) <$>) . parseJSON

instance FromJSON (Forest ByteString) where
  parseJSON = (((Text.encodeUtf8 <$>) <$>) <$>) . parseJSON

instance FromJSON (Tree String) where
  parseJSON = ((Text.unpack <$>) <$>) . parseJSON

instance FromJSON (Forest String) where
  parseJSON = (((Text.unpack <$>) <$>) <$>) . parseJSON

