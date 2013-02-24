{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving
           , TupleSections
           , ScopedTypeVariables
           , RecordWildCards
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
import qualified Data.Vector as Vector

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
   do scanned <- scanKeys body
      case scanned of [(ref, name)] -> Use ref <$> body .: name
                      _             -> mzero
instance ToJSON (Use Templated) where
  toJSON Use{..} = object [toStr task .= array (toJSON <$> args)]

instance FromJSON (Tree (Use Templated)) where
  parseJSON = withObject "Tree(TaskL.Use)" $ \body ->
   do deps <- body .:? "deps" .!= mempty
      Node <$> parseJSON (Object body) <*> mapM parseJSON deps
instance ToJSON (Tree (Use Templated)) where
  toJSON Node{..} | [] <- subForest = Object o
                  | otherwise       = Object (uncurry HashMap.insert deps o)
   where Object o = toJSON rootLabel
         deps     = "deps" .= array (toJSON <$> subForest)


-- Single element hashes are treated as variable references.
instance FromJSON [Either Label ByteString] where
  parseJSON (Array  a)   = mapM varOrLit (toList a)
  parseJSON v@(Object _) = (:[]) <$> varOrLit v
  parseJSON v@(String _) = (:[]) <$> varOrLit v
  parseJSON v@(Number _) = (:[]) <$> varOrLit v
  parseJSON _            = mzero
instance ToJSON [Either Label ByteString] where
  toJSON items = case items of
    [       ] -> String ""
    [Left l ] -> var l
    [Right b] -> val b
    _         -> Array . Vector.fromList $ either var val <$> items
   where var = object . (:[]) . (.= String "") . toStr
         val = String . Text.decodeUtf8

instance FromJSON Name where
  parseJSON = withText "Name" (either (const mzero) return . unStr)
instance ToJSON Name where toJSON = String . toStr

instance FromJSON Label where
  parseJSON = withText "Label" (either (const mzero) return . unStr)
instance ToJSON Label where toJSON = String . toStr

instance FromJSON (Code Templated) where
  parseJSON = withArray "TaskL.Code" ((Commands <$>) . mapM parseOne . toList)
   where parseOne = withArray "Task.Code/..." (cmd <=< mapM parseJSON . toList)
          where cmd (h:t) = return (TemplatedCmd h, t)
                cmd _     = mzero
instance ToJSON (Code Templated) where
  toJSON (Commands cmds) =
    array [ array (toJSON <$> cmd:args) | (TemplatedCmd cmd, args) <- cmds ]

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

array :: [Value] -> Value
array  = Array . Vector.fromList
