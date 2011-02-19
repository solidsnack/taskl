
module Language.TaskL.YAML where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)

import Data.Object.Yaml

import Language.TaskL.Syntax
import Language.TaskL.StringL


data NamedTaskParseResult =
  NamedTaskParseResult { name  :: Name
                       , body  :: TaskBody
                       , extra :: Map YamlScalar YamlObject }

bodyFromYAML                ::  [(YamlScalar, YamlObject)] -> ()
bodyFromYAML                 =  undefined

