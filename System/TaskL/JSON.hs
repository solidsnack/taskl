{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving
           , TupleSections
           , ScopedTypeVariables
           , FlexibleContexts
           , FlexibleInstances #-}
module System.TaskL.JSON where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Foldable (toList)
import           Data.Tree (Tree(..), Forest)

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import           Data.Text
import qualified Data.Text.Encoding as Text

import           System.TaskL.Phases
import           System.TaskL.Strings


instance FromJSON (Module Templated) where
  parseJSON = withObject "TaskL.Module" $ \body ->
   do names <- scanKeys body
      Module "" . Map.fromList <$>
        sequence [ (name,) <$> body .: text | (name, text) <- names ]

instance ToJSON (Module Templated) where
  toJSON _ = undefined

instance FromJSON Templated where
  parseJSON = withObject "TaskL.Task" template where
    template body = Templated <$> body .:? "vars" .!= mempty
                              <*> knot body
    knot body = Knot <$> body .:? "cmds" .!= Commands []
                     <*> body .:? "deps" .!= mempty
                     <*> body .:? "asks" .!= mempty

instance FromJSON (Use Templated) where
  parseJSON = withObject "TaskL.Use" $ \body ->
   do (ref, name) <- scanForRef body
      Use ref <$> body .: name

instance FromJSON (Tree (Use Templated)) where
  parseJSON = withObject "Tree(TaskL.Use)" $ \body ->
   do deps <- body .:? "deps" .!= mempty
      Node <$> parseJSON (Object body) <*> mapM parseJSON deps

-- Single element hashes are treated as variable references.
instance FromJSON [Either Label ByteString] where
  parseJSON (Array  a)   = mapM varOrLit (toList a)
  parseJSON v@(Object _) = (:[]) <$> varOrLit v
  parseJSON v@(String _) = (:[]) <$> varOrLit v
  parseJSON v@(Number _) = (:[]) <$> varOrLit v
  parseJSON _            = mzero

instance FromJSON Name where
  parseJSON (String s) = either (const mzero) return (unStr s)
  parseJSON _          = mzero

instance FromJSON Label where
  parseJSON (String s) = either (const mzero) return (unStr s)
  parseJSON _          = mzero

instance FromJSON (Code Templated) where
  parseJSON (Array a) = undefined
  parseJSON _         = mzero

scanForRef :: Object -> Parser (Ref Templated, Text)
scanForRef  = undefined

scanKeys :: (Str s Text) => Object -> Parser [(s, Text)]
scanKeys  = return . rights . (parse <$>) . HashMap.keys
 where parse text = (,text) <$> unStr text

varOrLit :: Value -> Parser (Either Label ByteString)
varOrLit (Object o) = case [ (unStr k, v) | (k, v) <- HashMap.toList o ] of
  [(Right l, String "")] -> return . Left $ l
  [(Right l, Null     )] -> return . Left $ l
  _                      -> mzero
varOrLit (String s) = return . Right . Text.encodeUtf8 $ s
varOrLit (Number n) = return . Right . ByteString.pack . show $ n
varOrLit _          = mzero

onlyOne :: [t] -> Parser t
onlyOne [x] = return x
onlyOne _   = mzero

