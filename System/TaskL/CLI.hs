{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
           , ScopedTypeVariables
           , TypeOperators
           , TypeFamilies
           , TupleSections
           , TemplateHaskell
           , FlexibleInstances
           , FlexibleContexts
           , RecordWildCards
           , TransformListComp
           , NoMonomorphismRestriction
           , UndecidableInstances #-}
-- TODO: Remove extensions and libraries.
module System.TaskL.CLI where

import           Prelude hiding (catch, mapM, any, concatMap, foldr)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Lazy as Lazy
import           Control.Arrow
import           Control.Applicative
import           Control.Exception
import           Control.Monad hiding (mapM)
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State.Strict as State
import           Data.Char
import           Data.Either
import           Data.Foldable hiding (sequence_)
import           Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import           Data.Traversable
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid hiding (All)
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree
import qualified GHC.Exts
import qualified Text.Printf as Printf
import           System.Exit
import           System.IO
import qualified System.Posix.Env.ByteString as Posix

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Builder as Builder
import           Data.FileEmbed
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Yaml
import qualified Text.Libyaml
import qualified Language.Bash as Bash

import           System.TaskL.Strings
import           System.TaskL.Task
import           System.TaskL.JSON
import           System.TaskL.Compiler


main :: IO ()
main  = do
  (mode, rest) <- modes =<< Posix.getArgs
  case mode of Help    -> help    >> exitSuccess
               Version -> version >> exitSuccess
               Normal  -> when (rest==[]) (err "No tasks requested :(")
  tasks <- lift "arguments:" parseTasks rest
  mod   <- merge =<< load [("<stdin>", stdin)]
  script "(unversioned)" tasks mod >>= ByteString.putStr
 where
  help    = ByteString.putStr readme
  version = ByteString.putStrLn "(unversioned)"


data Mode = Help | Version | Normal deriving (Eq, Ord, Show)

modes :: [ByteString] :~ (Mode, [ByteString])
modes  = mode' Normal
 where mode' m [   ] = return (m, [])
       mode' m (h:t) | "//" `ByteString.isPrefixOf` h    = return (m, h:t)
                     | h `List.elem` ["-h", "--help"]    = mode' Help t
                     | h `List.elem` ["-v", "--version"] = mode' Version t
                     | otherwise                         = err "Bad argument."

parseTasks :: [ByteString] :- [(Name, [ByteString])]
parseTasks args = mapM parse . snd
                $ foldr (maybe noSep withSep (listToMaybe sep)) ([],[]) args
 where noSep s (inProgress, complete) = if "//" `ByteString.isPrefixOf` s
         then ( [], (s, inProgress) : complete )
         else ( s:inProgress,         complete )
       withSep sep s (inProgress, complete) = if s /= sep
         then (s:inProgress, complete)
         else case inProgress of h:t -> ([], (h,t):complete)
                                 [ ] -> ([], complete)
       sep = [ s | s <- args, isSep s, then order by (ByteString.length s) ]
        where isSep s = ByteString.all (=='/') s && ByteString.length s >= 2
              order   = GHC.Exts.sortWith
       parse (b, bs)  = (,bs) <$> left Text.pack (unStr b)
       left f = either (Left . f) Right

readme :: ByteString
readme  = $(embedFile "README")

