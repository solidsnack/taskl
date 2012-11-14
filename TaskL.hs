{-# LANGUAGE OverloadedStrings
           , TupleSections
           , TemplateHaskell
           , ScopedTypeVariables
           , GeneralizedNewtypeDeriving #-}
module TaskL where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
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

import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import           Data.FileEmbed
import           Data.Graph.Wrapper
import           Data.Yaml
import qualified Text.ShellEscape as Esc
import qualified Language.Bash as Bash

import           JSONTree


data Cmd = Cmd Program [Argument] deriving (Eq, Ord, Show)

data Program = ShHTTP ByteString | Path ByteString deriving (Eq, Ord, Show)

data Argument = Str ByteString deriving (Eq, Ord, Show)
instance IsString Argument where fromString = Str . ByteString.pack


-- | Render a command section to a Bash command line.
command :: Cmd -> Bash.Statement ()
command (Cmd p args) = case p of ShHTTP url -> bash "curl_sh" (Str url:args)
                                 Path path  -> bash path args
 where bash a b = Bash.SimpleCommand (Bash.literal a) (arg <$> b)

arg :: Argument -> Bash.Expression ()
arg (Str b) = Bash.literal b

-- | Calculate a linear schedule from a graph if there are no cycles, or
--   return the cycles.
schedule :: (Eq t) => Graph t t -> Either [[t]] [t]
schedule g | a == []   = Right b
           | otherwise = Left a
 where (a, b) = partitionEithers (scc2either <$> stronglyConnectedComponents g)
       scc2either (CyclicSCC ts) = Left ts
       scc2either (AcyclicSCC t) = Right t

-- | Generate Bash function declaration for task schedule.
tasks :: [Cmd] -> Bash.Statement ()
tasks  = Bash.Function "tasks" . anno . and . (command <$>)
 where anno = Bash.Annotated ()
       and cmds = case cmds of [   ] -> Bash.SimpleCommand "msg" ["No tasks."]
                               [cmd] -> cmd
                               cmd:t -> Bash.Sequence (anno cmd) (anno (and t))

script :: [Cmd] -> ByteString
script list = header <> Bash.bytes (tasks list) <> "\n" <> footer


-- | When input documents are read, they present tasks and dependencies as
--   trees. These trees are collapsed to an adjacency list representation, for
--   use by a graph library.
adjacencies :: Tree t -> [(t, [t])]
adjacencies (Node t [ ]) = (t, []) : []
adjacencies (Node t sub) = (t, Tree.rootLabel <$> sub)
                         : concatMap adjacencies sub

-- | A task could be present multiple times in a document or collection of
--   documents. Dependencies are always merged across multiple definitions of
--   a task, by taking the set union of them.
merge :: (Ord t) => [(t, [t])] -> Map t (Set t)
merge  = (Set.fromList <$>) . Map.fromListWith (++)

-- | Merge adjacency lists and produce a graph.
graph :: (Ord t) => [(t, [t])] -> Graph t t
graph  = fromListSimple . Map.toAscList . (Set.toAscList <$>) . merge


frame, header, footer :: ByteString
frame            = $(embedFile "frame.bash")
(header, footer) = (ByteString.unlines *** ByteString.unlines)
                 . second (drop 1 . dropWhile (/= "}"))
                 . span (/= "function tasks {")
                 $ ByteString.lines frame


data Definition = Definition Name [Cmd] [Name]

newtype Name = Name ByteString deriving (Eq, Ord, Show, IsString)

program :: Attoparsec.Parser Program
program  = (ShHTTP <$> url) <|> (Path <$> Attoparsec.takeByteString)

url :: Attoparsec.Parser ByteString
url  = (<>) <$> (Attoparsec.string "http://" <|> Attoparsec.string "https://")
            <*> Attoparsec.takeByteString

name :: Attoparsec.Parser Name
name  = (Name . ByteString.intercalate "." <$> labels) <* Attoparsec.endOfInput
 where labels = Attoparsec.sepBy1 label (Attoparsec.char '.')

call :: Attoparsec.Parser Name
call  = Attoparsec.string "//" *> name

label :: Attoparsec.Parser ByteString
label  = label' <|> ByteString.singleton <$> ld
 where ldu = Attoparsec.takeWhile1 (Attoparsec.inClass "a-zA-Z0-9_")
       ld  = Attoparsec.satisfy (Attoparsec.inClass "a-zA-Z0-9")
       label' = do b <- ByteString.cons <$> ld <*> ldu
                   if ByteString.last b == '_' then mzero else return b

task :: Tree ByteString -> Either String (Either Name Cmd)
task (Node s ts)  =  either Left (Right . (`Cmd` (Str . rootLabel <$> ts)))
                 <$> Attoparsec.parseOnly (Attoparsec.eitherP call program) s

definition :: Tree ByteString -> Either (ByteString, String) Definition
definition (Node s ts) = either (Left . (s,)) Right $ do
  whoami              <- Attoparsec.parseOnly name s
  (names, commands)   <- partitionEithers <$> mapM task ts
  return $ Definition whoami commands names

--definition (Node s ts) = either (s,) id $ do
--  whoami              <- Attoparsec.parseOnly name s
--  (names, commands)   <- partitionEithers <$> mapM task ts
--  return $ Definition whoami commands names


compiledModule :: Forest ByteString
               -> ((Map Name [Cmd], Graph Name Name), [(ByteString, String)])
compiledModule trees =
  ( (Map.fromList bodies, graph (leaves ++ dependencies)), failed )
 where (failed, defined) = partitionEithers (definition <$> trees)
       (bodies, dependencies) = unzip [ ((name, body), (name, names))
                                      | Definition name body names <- defined ]
       leaves = (,[]) <$> (Set.toList . missing . merge) dependencies
       missing m = Set.difference (Set.unions $ Map.elems m) (Map.keysSet m)


main :: IO ()
main = do
  return ()
--  args <- getArgs
--  Just (decls :: Forest ByteString) <- decode <$> ByteString.hGetContents stdin
--  let task | h:_ <- args = ByteString.pack h
--           | otherwise   = "//"
--      parses = declaration <$> decls
--      g = graph (backToTree <$> rights parses)
--  case script <$> schedule g of
--    Right bash  -> ByteString.putStr bash
--    Left cycles -> do hPutStrLn stderr "Not able to schedule due to cycles:"
--                      (hPutStrLn stderr . show) `mapM_` cycles

