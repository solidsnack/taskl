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
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid
import           Data.Tree (Tree(..), Forest)
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
import           System.TaskL.Phases
import           System.TaskL.JSON


-- | The type of compiler actions, including loading files, parsing, internal
--   AST transformations and the eventual compilation to Bash.
type a :~ b = a -> IO b

 -------------------------------- Loading Files -------------------------------

openModule :: FilePath :~ (ByteString, Handle)
openModule path = (ByteString.pack path,) <$> openFile path ReadMode

loadModule :: (ByteString, Handle) :~ Module Templated
loadModule (name, handle) = do
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
    ((k, Right _), Left _)  -> Left $ msg' ("Bad task body for: " <> k)
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

renderModule :: (Aeson.ToJSON (Module t)) => Module t :~ ()
renderModule m@Module{..} = do when (from /= mempty) (out comment)
                               out (Data.Yaml.encode m)
 where comment = ByteString.unlines (("# " <>) <$> ByteString.lines from)

 ------------------------------ Compiling Stuff -------------------------------

template :: Module Templated -> Map Name ByteString :~ Module WithURLs
template  = undefined

remotes :: Bool -- ^ If true, retrieve remote commands at compile time and
                --   inline them; otherwise, generate shell code to retrieve
                --   the commands with cURL and execute them.
        -> Module WithURLs :~ Module Static
remotes  = undefined

bash :: Module Static -> Bash.Annotated ()
bash  = undefined

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

