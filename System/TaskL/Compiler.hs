{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
           , ScopedTypeVariables
           , TypeOperators
           , TypeFamilies
           , FlexibleInstances
           , RecordWildCards
           , UndecidableInstances #-}
module System.TaskL.Compiler where

import           Prelude hiding (catch)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Control.Arrow
import           Control.Applicative
import           Control.Exception
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

loadYAML :: FilePath :~ Module Templated
loadYAML path = do
  res :: Maybe Aeson.Value <- catch (Data.Yaml.decodeFile path) yamlErr
  halfParsed <- case res of Nothing -> err' "No YAML data!"
                            Just (Data.Yaml.Object o) -> return (halfParse o)
                            Just _ -> err' "YAML root should be a map."
  case partitionEithers (partition <$> halfParsed) of
    ([      ], [  ]) -> return (Module path' mempty)
    (warnings, [  ]) -> sequence_ warnings >> err' "No tasks were valid :("
    (warnings, defs) -> Module path' (Map.fromList defs) <$ sequence_ warnings
 where err'       :: String -> IO any
       err'        = err . (<>) (path <> ": ")
       msg'       :: String -> IO ()
       msg'        = msg . (<>) (path <> ": ")
       path'       = ByteString.pack path
       yamlErr exc = err' (yamlErrorInfo exc)
       halfParse o = p <$> HashMap.toList o
        where p = ((Text.unpack &&& unStr) *** eitherJSON)
       eitherJSON json = case Aeson.fromJSON json of Aeson.Error s   -> Left s
                                                     Aeson.Success v -> Right v
       partition result = case result of
         ((k, Left  _), _)       -> Left $ msg' ("Bad task name: " <> k)
         ((k, Right _), Left _)  -> Left $ msg' ("Bad task body for: " <> k)
         ((_, Right n), Right b) -> Right (n, b)

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


template :: Module Templated -> Map Name ByteString :~ Module WithURLs
template  = undefined

remotes :: Bool -- ^ If true, retrieve remote commands at compile time and
                --   inline them; otherwise, generate shell code to retrieve
                --   the commands with cURL and execute them.
        -> Module WithURLs :~ Module Static
remotes  = undefined

bash :: Module Static -> Bash.Annotated ()
bash  = undefined


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

