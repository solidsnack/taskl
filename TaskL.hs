
module TaskL where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Either
import           Data.Monoid
import           Data.String

import           Data.Graph.Wrapper


data Task = Task Command [Argument]
 deriving (Eq, Ord, Show)

data Command = ShHTTP ByteString | Path ByteString
 deriving (Eq, Ord, Show)

data Argument = Literal ByteString | Expandable ByteString
 deriving (Eq, Ord, Show)
instance IsString Argument where
  fromString = Literal . ByteString.pack

command :: Command -> [Argument] -> [Argument]
command (ShHTTP url) args = "curl_sh" : Literal url : args
command (Path path)  args = Literal path : args

-- | Attempts to schedule a task graph. If there are cycles, scheduling fails
--   and the cycles are returned.
schedule :: Graph Task Task -> Either [[Task]] [Task]
schedule g | a == []   = Right b
           | otherwise = Left a
 where (a, b) = partitionEithers (scc2either <$> stronglyConnectedComponents g)
       scc2either (CyclicSCC ts) = Left ts
       scc2either (AcyclicSCC t) = Right t

