{-# LANGUAGE OverloadedStrings
           , TupleSections
           , RecordWildCards
           , FlexibleContexts
           , PatternGuards
           , FlexibleInstances #-}
module System.TaskL.JSON where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid hiding (All)
import           Data.Foldable (toList)
import           Data.Tree (Tree(..), Forest)

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector

import           System.TaskL.Task
import           System.TaskL.Strings


instance FromJSON Module where
  parseJSON = withObject "TaskL.Module" $ \body ->
   do names <- scanKeys body
      Module "" . Map.fromList <$>
        sequence [ (name,) <$> body .: text | (name, text) <- names ]
instance ToJSON Module where
  toJSON Module{..} = object [ toStr k .= toJSON v | (k,v) <- Map.toList defs ]

instance FromJSON Task where
  parseJSON = withObject "TaskL.Task" template where
    template body = Task <$> body .:? "vars" .!= mempty
                         <*> knot body
    knot body = Knot <$> body .:? "cmds" .!= Commands []
                     <*> body .:? "deps" .!= mempty
instance ToJSON Task where
  toJSON (Task vars Knot{..}) = object $ filter nonEmpty
    [ "deps" .= array (toJSON <$> deps),
      "vars" .= array (toJSON <$> vars), "cmds" .= toJSON code ]
   where nonEmpty (_, Array a) = a /= mempty
         nonEmpty _            = False

instance FromJSON Use where
  parseJSON (Object o) = do
    scanned <- scanKeys o
    case scanned of [(ref, name)] -> Use ref <$> o .: name
                    _             -> mzero
  parseJSON (String s) = flip Use [] <$> parseJSON (String s)
  parseJSON _          = mzero
instance ToJSON Use where
  toJSON Use{..} = object [toStr task .= array (toJSON <$> args)]

instance FromJSON (Tree Use) where
  parseJSON (Object o) = do
    deps <- o .:? "deps" .!= mempty
    Node <$> parseJSON (Object o) <*> mapM parseJSON deps
  parseJSON (String s) = flip Node [] <$> parseJSON (String s)
  parseJSON _          = mzero
instance ToJSON (Tree Use) where
  toJSON Node{..} | [] <- subForest = Object o
                  | otherwise       = Object (uncurry HashMap.insert deps o)
   where Object o = toJSON rootLabel
         deps     = "deps" .= array (toJSON <$> subForest)

instance FromJSON (Label, Maybe ByteString) where
  parseJSON (Object o) = case [ (unStr k, v) | (k, v) <- HashMap.toList o ] of
    [(Right l, String s)] -> return (l, Just (Text.encodeUtf8 s))
    [(Right l, Number n)] -> return (l, Just (ByteString.pack $ show n))
    _                     -> mzero
  parseJSON (String s) = (,Nothing) <$> parseJSON (String s)
  parseJSON _          = mzero
instance ToJSON (Label, Maybe ByteString) where
  toJSON (l, Nothing) = String (toStr l)
  toJSON (l, Just b)  = object [toStr l .= String (Text.decodeUtf8 b)]

-- Single element hashes are treated as variable references.
instance FromJSON Arg where
  parseJSON (Array  a)   = Scalar <$> mapM varOrLit (toList a)
  parseJSON v@(String _) = Scalar . (:[]) <$> varOrLit v
  parseJSON v@(Number _) = Scalar . (:[]) <$> varOrLit v
  parseJSON v@(Object o) = case HashMap.toList o of
                             [("...", Null)] -> return Tail
                             [("~~~", Null)] -> return All
                             _               -> Scalar . (:[]) <$> varOrLit v
  parseJSON _            = mzero
instance ToJSON Arg where
  toJSON (Scalar items) = case items of
    [       ] -> String ""
    [Left l ] -> var l
    [Right b] -> val b
    _         -> Array . Vector.fromList $ either var val <$> items
   where var = object . (:[]) . (.= Null) . toStr
         val = String . Text.decodeUtf8
  toJSON Tail = object ["..." .= Null]
  toJSON All  = object ["~~~" .= Null]

instance FromJSON Name where
  parseJSON = withText "Name" (either (const mzero) return . unStr)
instance ToJSON Name where toJSON = String . toStr

instance FromJSON Label where
  parseJSON = withText "Label" (either (const mzero) return . unStr)
instance ToJSON Label where toJSON = String . toStr

instance FromJSON Code where
  parseJSON = withArray "TaskL.Code" ((Commands <$>) . mapM parseOne . toList)
   where parseOne = withArray "Task.Code/..." (argv . toList)
          where argv (h:t) = (,) <$> cmd h <*> mapM parseJSON t
                argv [   ] = mzero
                cmd v  =  (:[]) <$> varOrLit v
                      <|> withArray "Task.Code/..." (mapM varOrLit . toList) v
instance ToJSON Code where
  toJSON (Commands cmds) =
    array [ array (toJSON cmd : (toJSON <$> args)) | (cmd, args) <- cmds ]

scanKeys :: (Str s Text) => Object -> Parser [(s, Text)]
scanKeys  = return . rights . (parse <$>) . HashMap.keys
 where parse text = (,text) <$> unStr text

varOrLit :: Value -> Parser (Either Label ByteString)
varOrLit (Object o) = case [ (unStr k, v) | (k, v) <- HashMap.toList o ] of
  [(Right l, Null)] -> return . Left $ l
  _                 -> mzero
varOrLit (String s) = return . Right . Text.encodeUtf8 $ s
varOrLit (Number n) = return . Right . ByteString.pack . show $ n
varOrLit _          = mzero

onlyOne :: [t] -> Parser t
onlyOne [x] = return x
onlyOne _   = mzero

array :: [Value] -> Value
array  = Array . Vector.fromList
