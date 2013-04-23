{-# LANGUAGE OverloadedStrings
           , FlexibleInstances
           , MultiParamTypeClasses
           , FlexibleContexts
           , UndecidableInstances #-}
-- | Utilities for plain text and the restricted syntax of names for tasks and
--   labels.
module System.TaskL.Strings where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.String
import           Data.Monoid

import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text


-- | Dots and labels.
newtype Name  = Name [Label] deriving (Eq, Ord, Show)
instance IsString Name where fromString s = l where (Right l) = unStr s

-- | A string matching @[a-z]([a-zA-Z0-9_]*[a-zA-Z0-9])?@, a syntax
--   compatible with variable names in many languages.
newtype Label = Label ByteString deriving (Eq, Ord, Show)
instance IsString Label where fromString s = l where (Right l) = unStr s

class Str t s where
  unStr :: s -> Either String t
  toStr :: t -> s
instance Str Name ByteString where
  unStr = parseCompletely name
  toStr (Name labels) = "//" <> ByteString.intercalate "." (toStr <$> labels)
instance Str Label ByteString where
  unStr = parseCompletely label
  toStr (Label b) = b
instance Str t ByteString => Str t String where
  unStr = unStr . ByteString.pack
  toStr = ByteString.unpack . toStr
instance Str t ByteString => Str t Text where
  unStr = unStr . Text.encodeUtf8
  toStr = Text.decodeUtf8 . toStr

-- | Completely parse the input 'ByteString' using the given parser.
parseCompletely :: Attoparsec.Parser a -> ByteString -> Either String a
parseCompletely p = Attoparsec.parseOnly (p <* Attoparsec.endOfInput)


url :: Attoparsec.Parser ByteString
url  = (<>) <$> (Attoparsec.string "http://" <|> Attoparsec.string "https://")
            <*> Attoparsec.takeByteString

name :: Attoparsec.Parser Name
name  = Attoparsec.string "//" *> (Name <$> labels)
 where labels = Attoparsec.sepBy1 label (Attoparsec.char '.')

label :: Attoparsec.Parser Label
label  = Label <$> do
  joined <- (<>) <$> scan "a-z" <*> Attoparsec.option "" (scan "a-zA-Z0-9_-")
  joined <$ guard (ByteString.last joined /= '_')
 where scan pattern = Attoparsec.takeWhile1 (Attoparsec.inClass pattern)

utf8 :: Text -> ByteString
utf8  = Text.encodeUtf8

