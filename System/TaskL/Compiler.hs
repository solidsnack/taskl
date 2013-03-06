{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
           , ScopedTypeVariables
           , TypeOperators
           , TypeFamilies
           , TupleSections
           , FlexibleInstances
           , FlexibleContexts
           , RecordWildCards
           , UndecidableInstances #-}
module System.TaskL.Compiler where

import           Prelude hiding (catch)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Control.Arrow
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Foldable hiding (sequence_)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tree (Tree(..), Forest)
import qualified Data.Tree as Tree
import           System.Exit
import           System.IO

import qualified Data.Aeson as Aeson
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Yaml
import qualified Text.Libyaml
import qualified Language.Bash as Bash

import           System.TaskL.Strings
import           System.TaskL.Task
import           System.TaskL.JSON


-- | The type of compiler actions, including loading files, parsing, internal
--   AST transformations and the eventual compilation to Bash.
type a :~ b = a -> IO b

 ----------------------------- Compilation Steps ------------------------------

load :: [(ByteString, Handle)] :~ [Module]
load  = mapM loadYAML

merge :: [Module] :~ Module
merge modules = return Module{ from = froms, defs = map }
 where (froms, map) = foldl' f ("",mempty) modules
       f (s, map) Module{..} = (s <> from <> "\n", map <> defs)

filter :: Set Name -> Module :~ Module
filter set mod@Module{..} = return mod{ defs = filtered }
 where requested k _ = Set.member k (reachable set defs)
       filtered = Map.filterWithKey requested defs

-- | Plans a task request, returning a tree of calls to needed tasks.
expand :: [(Name, [ByteString])] -> Module :~ Tree (Name, [ByteString])
expand requests Module{..} = undefined

bodies :: Module :~ Map Name (Bash.Annotated ())
bodies  = undefined

check :: Tree (Name, [ByteString]) :~ ()
check  = undefined

bash :: Map Name (Bash.Annotated ()) -> Tree (Name, [ByteString])
     :~ Bash.Annotated ()
bash  = undefined

 ---------------------- Work With Bindings & Request Lists --------------------

reachable :: Set Name -> Map Name Task -> Set Name
reachable requested available = search requested
 where onlyNames  = names <$> available
       subTasks k = maybe mempty id (Map.lookup k onlyNames)
       search :: Set Name -> Set Name
       search set = if next == set then set else search next
        where next = fold (set : (subTasks <$> toList set))

names :: Task -> Set Name
names (Task _ Knot{..}) = fold [ toSet tree | tree <- asks <> deps ]
 where toSet tree = Set.fromList $ Tree.flatten (task <$> tree)

-- | Bind vector of arguments to variables and return leftover variables (if
--   there are too few arguments) or arguments (if there are too few
--   variables).
bind :: [ByteString] -> [(Label, Maybe ByteString)]
     -> (Map Label ByteString, Either [(Label, Maybe ByteString)] [ByteString])
bind values variables = (Map.fromList found, leftover)
 where (found, leftover) = f [] values variables
       f found (h:t) ((l, _)     :labels)    = f ((l, h):found) t  labels
       f found [   ] ((l, Just b):labels)    = f ((l, b):found) [] labels
       f found [   ] labels@((_, Nothing):_) = (found, Left labels)
       f found rest  [ ]                     = (found, Right rest)

 --------------------------------- Utilities ----------------------------------

-- | Functions for informational, diagnostic and warning messages published by
--   the compiler pipeline.
msg, out :: (Echo t) => t :~ ()
msg = echo stderr
out = echo stdout

-- | A way to signal an error in the compiler pipeline, terminating it and
--   providing a message.
err :: (Echo t) => t :~ any
err = (>> exitFailure) . echo stderr

class Echo t where echo :: Handle -> t -> IO ()
instance Echo ByteString where
  echo h = ByteString.hPutStrLn h . fst . ByteString.spanEnd isSpace
instance Echo Text where echo h = Text.hPutStrLn h . Text.stripEnd
instance Echo String where echo h = echo h . Text.pack

 -------------------------------- Loading Files -------------------------------

openModule :: FilePath :~ (ByteString, Handle)
openModule path = (ByteString.pack path,) <$> openFile path ReadMode

loadYAML :: (ByteString, Handle) :~ Module
loadYAML (name, handle) = do
  parseResult <- yamlDecode =<< ByteString.hGetContents handle
  mapping     <- yamlProcessErrors parseResult
  case partitionEithers (partition <$> halfParse mapping) of
    ([      ], [  ]) -> return (Module name mempty)
    (warnings, [  ]) -> sequence_ warnings >> err' "No tasks were valid :("
    (warnings, defs) -> Module name (Map.fromList defs) <$ sequence_ warnings
 where
  err'       :: String -> IO any
  err'        = err . (<>) (ByteString.unpack name <> ": ")
  msg'       :: String -> IO ()
  msg'        = msg . (<>) (ByteString.unpack name <> ": ")
  halfParse o = ((Text.unpack &&& unStr) *** eitherJSON) <$> HashMap.toList o
  eitherJSON json = case Aeson.fromJSON json of
    Aeson.Error s   -> Left s
    Aeson.Success v -> Right v
  partition result = case result of
    ((k, Left  _), _)       -> Left $ msg' ("Bad task name: " <> k)
    ((k, Right _), Left s)  -> Left $ msg' ("Bad task body for: "<>k<>"\n "<>s)
    ((_, Right n), Right b) -> Right (n, b)
  yamlProcessErrors yamlProcessingResult = case yamlProcessingResult of
     Left exc                           -> err' (yamlErrorInfo exc)
     Right (Left s)                     -> err' ("YAML Error: " <> s)
     Right (Right (Data.Yaml.Object o)) -> return o
     Right (Right _)                    -> err' "YAML root should be a map."

yamlDecode :: ByteString -> IO (Either Data.Yaml.ParseException
                                       (Either String Aeson.Value))
yamlDecode  = Data.Yaml.decodeHelper . Text.Libyaml.decode

yamlErrorInfo :: Data.Yaml.ParseException -> String
yamlErrorInfo Data.Yaml.NonScalarKey =
  "Please use only scalar keys (numbers or strings)."
yamlErrorInfo (Data.Yaml.UnknownAlias alias) = "Missing alias: " <> alias
yamlErrorInfo (Data.Yaml.UnexpectedEvent a b) = unlines
  [ "Unexpected item in YAML event stream:", "  " <> show a,
    "while expecting:",                      "  " <> show b ]
yamlErrorInfo (Data.Yaml.InvalidYaml exc) = case exc of
  Nothing -> "The YAML was invalid in some way."
  Just (Text.Libyaml.YamlException s) -> "The YAML was invalid:\n  " <> s
  Just (Text.Libyaml.YamlParseException{..}) ->
    "The YAML was invalid (at " <> i yamlProblemMark <> "):\n  " <> yamlProblem
    where i Text.Libyaml.YamlMark{..} = show yamlLine <>":"<> show yamlColumn

 ------------------------------ Pretty Printing -------------------------------

renderModule :: (Aeson.ToJSON Module) => Module :~ ()
renderModule m@Module{..} = do when (from /= mempty) (out comment)
                               out (Data.Yaml.encode m)
 where comment = ByteString.unlines (("# " <>) <$> ByteString.lines from)

