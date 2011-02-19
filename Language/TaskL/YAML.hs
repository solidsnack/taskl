
module Language.TaskL.YAML where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)

import Data.Object.Yaml

import Language.TaskL.Syntax
import Language.TaskL.StringL


data TaskParseResult =
  TaskParseResult { body    ::  TaskBody
                  , extra   ::  Map YamlScalar YamlObject }

bodyFromYAML                ::  [(YamlScalar, YamlObject)] -> ()
bodyFromYAML                 =  undefined


readMapToModule :: Map YamlScalar YamlObject -> Map Name TaskParseResult
readMapToModule              =  undefined



