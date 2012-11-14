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
import           Data.Graph.Wrapper (Graph)
import           Data.Graph.Wrapper as Graph
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
 where (a, b) = partitionEithers
                (scc2either <$> Graph.stronglyConnectedComponents g)
       scc2either (Graph.CyclicSCC ts) = Left ts
       scc2either (Graph.AcyclicSCC t) = Right t

-- | Generate Bash function declaration for task schedule.
tasks :: [Cmd] -> Bash.Statement ()
tasks  = Bash.Function "tasks" . anno . and . (command <$>)
 where anno = Bash.Annotated ()
       and cmds = case cmds of [   ] -> Bash.SimpleCommand "msg" ["No tasks."]
                               [cmd] -> cmd
                               cmd:t -> Bash.Sequence (anno cmd) (anno (and t))

script :: [Cmd] -> ByteString
script list = header <> Bash.bytes (tasks list) <> "\n" <> footer


-- | A task name could be present multiple times in a document or collection
--   of documents (for example, the @_@ default task). The "leftmost"
--   (earliest) definition overrides later definitions.
merge :: (Ord t) => [(t, [t])] -> Map t (Set t)
merge  = (Set.fromList <$>) . Map.fromListWith const

-- | Convert from a 'Map' of adjacencies to a list of adjacencies.
adjacencies :: (Ord t) => Map t (Set t) -> [(t, [t])]
adjacencies  = Map.toAscList . (Set.toAscList <$>)

-- | Merge adjacency lists and produce a graph.
graph :: (Ord t) => Map t (Set t) -> Graph t t
graph  = Graph.fromListSimple . adjacencies


frame, header, footer :: ByteString
frame            = $(embedFile "frame.bash")
(header, footer) = (ByteString.unlines *** ByteString.unlines)
                 . second (drop 1 . dropWhile (/= "}"))
                 . span (/= "function tasks {")
                 $ ByteString.lines frame


data Definition = Definition Name [Cmd] [Name]

newtype Name = Name ByteString deriving (Eq, Ord, Show, IsString)

data Module = Module (Map Name [Cmd]) (Map Name (Set Name))
 deriving (Eq, Ord, Show)

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

-- | Parse a definition. Note that one may define @_@ (taken to be the default
--   if a module is loaded but no specific task is asked for) but that name is
--   not callable.
definition :: Tree ByteString -> Either (ByteString, String) Definition
definition (Node s ts) = either (Left . (s,)) Right $ do
  whoami              <- Attoparsec.parseOnly (main <|> name) s
  (names, commands)   <- partitionEithers <$> mapM task ts
  return $ Definition whoami commands names
 where
  main = Name <$> Attoparsec.string "_"

-- | Compile any number of loaded, semi-structured text trees to a module. Any
--   trees that fail to parse are returned in a separate list, to be used for
--   warnings. (At a later stage, absent definitions can result in compiler
--   failure.)
compiledModule :: Forest ByteString -> (Module, [(ByteString, String)])
compiledModule trees = (mod, failed)
 where (failed, defined) = partitionEithers (definition <$> trees)
       (bodies, dependencies) = unzip [ ((name, body), (name, names))
                                      | Definition name body names <- defined ]
       leaves    = (,[]) <$> (Set.toList . missing . merge) dependencies
       missing m = Set.difference (Set.unions $ Map.elems m) (Map.keysSet m)
       mod       = Module (Map.fromList bodies) (merge (leaves ++ dependencies))

-- | Clip an adjacency list to include only those entries reachable from the
--   given key. If no key is given, then the key @_@ is used if it is
--   in the map; otherwise, the entire adjacency list is returned.
--
--   If a key is given and it is not in the map, then 'Nothing' is returned.
subMap :: Maybe Name -> Map Name (Set Name) -> Maybe (Map Name (Set Name))
subMap name m = maybe (get "_" <|> Just m) get name
 where get key = reachable <$ guard (Map.member key m)
        where keys      = Set.fromList $ Graph.reachableVertices (graph m) key
              reachable = Map.fromList
                          [ (k,v) | (k,v) <- Map.toList m, Set.member k keys ]

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

