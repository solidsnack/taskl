{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , ScopedTypeVariables
           , GeneralizedNewtypeDeriving #-}
module TaskL where

import           Control.Applicative
import           Control.Arrow
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Either
import           Data.Monoid
import           Data.String
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree
import           System.IO
import           System.Environment

import           Data.FileEmbed
import           Data.Graph.Wrapper
import           Data.Yaml
import qualified Text.ShellEscape as Esc
import qualified Language.Bash as Bash

import           JSONTree


data Task = Cmd Command [Argument] -- ^ A command to run.
          | Msg Name               -- ^ Marks completion of a named task.
             deriving (Eq, Ord, Show)

data Command = ShHTTP ByteString | Path ByteString deriving (Eq, Ord, Show)

data Argument = Literal ByteString deriving (Eq, Ord, Show)
instance IsString Argument where fromString = Literal . ByteString.pack

newtype Name = Name ByteString deriving (Eq, Ord, Show, IsString)


-- | Render a command section to a Bash command line.
command :: Command -> [Argument] -> Bash.Statement ()
command cmd args = case cmd of ShHTTP url -> bash "curl_sh" (Literal url:args)
                               Path path  -> bash path args
 where bash a b = Bash.SimpleCommand (Bash.literal a) (arg <$> b)

-- | Render a task to an argument vector. Uses 'command' for commands and
--   inserts a message, @..: job.name@, for completed jobs.
compile :: Task -> Bash.Statement ()
compile (Cmd cmd args) = command cmd args
compile (Msg (Name b)) = Bash.SimpleCommand "msg" [Bash.literal (":)  "<>b)]

arg :: Argument -> Bash.Expression ()
arg (Literal b) = Bash.literal b

-- | Attempts to schedule a task graph. If there are cycles, scheduling fails
--   and the cycles are returned.
schedule :: Graph Task Task -> Either [[Task]] [Task]
schedule g | a == []   = Right b
           | otherwise = Left a
 where (a, b) = partitionEithers (scc2either <$> stronglyConnectedComponents g)
       scc2either (CyclicSCC ts) = Left ts
       scc2either (AcyclicSCC t) = Right t

-- | Generate Bash function declaration for task schedule.
tasks :: [Task] -> Bash.Statement ()
tasks  = Bash.Function "tasks" . anno . and . (compile <$>)
 where anno = Bash.Annotated ()
       and cmds = case cmds of [   ] -> Bash.SimpleCommand "msg" ["No tasks."]
                               [cmd] -> cmd
                               cmd:t -> Bash.AndAnd (anno cmd) (anno (and t))


script :: [Task] -> ByteString
script list = header <> Bash.bytes (tasks list) <> "\n" <> footer


-- | When input documents are read, they present tasks and dependencies as
--   trees. These trees are collapsed to an adjacency list representation, for
--   use by a graph library.
adjacencies :: Tree Task -> [(Task, [Task])]
adjacencies (Node t [ ]) = (t, []) : []
adjacencies (Node t sub) = (t, Tree.rootLabel <$> sub)
                         : concatMap adjacencies sub

-- | A task could be present multiple times in a document or collection of
--   documents. Dependencies are always merged across multiple definitions of
--   a task, by taking the set union of them.
merge :: [(Task, [Task])] -> Map Task (Set Task)
merge  = (Set.fromList <$>) . Map.fromListWith (++)

-- | Once input documents are parsed, they present a forest of tasks. These
--   tasks are merged to form the final, compilable graph.
graph :: Forest Task -> Graph Task Task
graph  = fromListSimple . Map.toAscList . (Set.toAscList <$>)
       . merge . concatMap adjacencies


frame, header, footer :: ByteString
frame            = $(embedFile "frame.bash")
(header, footer) = (ByteString.unlines *** ByteString.unlines)
                 . second (drop 1 . dropWhile (/= "}"))
                 . span (/= "function tasks {")
                 $ ByteString.lines frame


main = do
  args <- getArgs
  Just (decls :: Forest String) <- decode <$> ByteString.hGetContents stdin
  let task | h:_ <- args = ByteString.pack h
           | otherwise   = "//"
  putStr (Tree.drawForest decls)

