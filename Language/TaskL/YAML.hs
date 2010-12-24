
module Language.TaskL.YAML where

import Data.ByteString

import Data.Yaml.YamlLight


unYAML                      ::  YamlLight -> [Tree Task]

toYAML                      ::  [Tree Task] -> YamlLight


