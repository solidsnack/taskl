
module TaskL where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Monoid


data Task     = Task Command [Argument] [Task]
deriving (Eq, Ord, Show)

data Command  = ShHTTP ByteString | Path ByteString
deriving (Eq, Ord, Show)

data Argument = Literal ByteString | Expandable ByteString
deriving (Eq, Ord, Show)


command :: Command -> [Argument] -> [Argument]
command (ShHTTP url) args = "curl_sh":url:args 
command (Path path)  args = path:args

