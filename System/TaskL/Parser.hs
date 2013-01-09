{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , PatternGuards
           , GeneralizedNewtypeDeriving
           , StandaloneDeriving
           , TupleSections
           , ScopedTypeVariables
  #-}

module System.TaskL.Parser where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Either
import           Data.Foldable (toList)
import           Data.Maybe
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree

import           Data.Aeson (parseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as Vector
import qualified Data.Yaml as Yaml


data Declaration = Declaration ByteString Definition deriving (Eq, Ord, Show)

data Definition = Definition [Var] [Tree Call] deriving (Eq, Ord, Show)

data Call = Call (Type, ByteString) [TemplateString] deriving (Eq, Ord, Show)

deriving instance Ord t => Ord (Tree t)

data Type = Task | Path | HTTP deriving (Eq, Ord, Show)

data Var = Var ByteString (Maybe ByteString)
 deriving (Eq, Ord, Show)

newtype TemplateString = TemplateString [Either Var ByteString]
 deriving (Eq, Ord, Show)


declarations :: Aeson.Value -> Aeson.Parser [Declaration]
declarations (Aeson.Object o) = undefined
declarations _                = mzero

instance Aeson.FromJSON Definition where
  parseJSON json@(Aeson.Object o) = Definition [] <$> parseForest json
   where vars = HashMap.lookup "_" o
  parseJSON _ = mzero

instance Aeson.FromJSON Call where
  parseJSON (Aeson.Object o) = maybe mzero call (listToMaybe cmds)
   where cmds    =  parseableKeys cmdStr o
         cmdStr  =  (Task,) <$> name
                <|> (HTTP,) <$> url
                <|> (Path,) <$> Attoparsec.takeByteString
         call ((t,b), Aeson.Array v) = Call (t,b) <$> mapM parseJSON (toList v)
         call _                      = mzero
  parseJSON _ = mzero

instance Aeson.FromJSON TemplateString where
  parseJSON json = case json of
    Aeson.String t -> return $ TemplateString [bytes t]
    Aeson.Object _ -> TemplateString . (:[]) <$> pick json
    Aeson.Array v  -> TemplateString <$> mapM pick (toList v) 
    _              -> mzero
   where bytes = Right . Text.encodeUtf8
         pick (Aeson.String t) = return $ bytes t
         pick (Aeson.Object o) = case parseableKeys label o of
                                   [(t,_)] -> return . Left $ Var t Nothing
                                   _       -> mzero
         pick _                = mzero

parseTree :: Aeson.Value -> Aeson.Parser (Tree Call)
parseTree json@(Aeson.Object _) = Node <$> parseJSON json <*> parseForest json
parseTree _                     = mzero

parseForest :: Aeson.Value -> Aeson.Parser [Tree Call]
parseForest (Aeson.Object o)
  | Just _ <- forest >> branch     = mzero -- Fail if both are present.
  | Just (Aeson.Array v) <- forest = parseTrees v
  | Just (Aeson.Array v) <- branch = biasForest <$> parseTrees v
  | otherwise                      = mzero
 where branch = HashMap.lookup ";" o
       forest = HashMap.lookup "." o
       parseTrees = mapM parseTree . toList
       biasForest (n : Node l f : rest) = biasForest (Node l (n:f) : rest)
       biasForest list                  = list
parseForest _ = mzero


-- | Scan a JSON object for keys that match a certain parser.
parseableKeys :: Attoparsec.Parser t -> Aeson.Object -> [(t, Aeson.Value)]
parseableKeys p m = (snd . partitionEithers) (parse <$> HashMap.toList m)
 where parse (k, v) = (,v) <$> Attoparsec.parseOnly p (Text.encodeUtf8 k)

 ----------------- Parsing (input in raw and semi-raw forms) ------------------

url :: Attoparsec.Parser ByteString
url  = (<>) <$> (Attoparsec.string "http://" <|> Attoparsec.string "https://")
            <*> Attoparsec.takeByteString

name :: Attoparsec.Parser ByteString
name  = (<>) <$> Attoparsec.string "//"
             <*> (ByteString.intercalate "." <$> labels)
             <*  Attoparsec.endOfInput
 where labels = Attoparsec.sepBy1 label (Attoparsec.char '.')

label :: Attoparsec.Parser ByteString
label  = label' <|> ByteString.singleton <$> ld
 where ldu = Attoparsec.takeWhile1 (Attoparsec.inClass "a-zA-Z0-9_")
       ld  = Attoparsec.satisfy (Attoparsec.inClass "a-zA-Z0-9")
       label' = do b <- ByteString.cons <$> ld <*> ldu
                   if ByteString.last b == '_' then mzero else return b

