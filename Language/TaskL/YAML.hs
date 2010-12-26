
module Language.TaskL.YAML where

import Data.Tree
import qualified Data.Set as Set
import Data.Set (Set)
import Data.ByteString.Char8 hiding (map)

import qualified Data.Object as YO
import qualified Data.Object.Yaml as YO

import Language.TaskL.EncDec
import Language.TaskL.Task
import Language.TaskL.IdemShell
import Language.TaskL.Codegen.IdemShell (modeSymbolic)


decodeTasks                 ::  YO.YamlObject -> [Tree Task]
decodeTasks                  =  undefined

encodeTasks                 ::  [Tree Task] -> YO.YamlObject
encodeTasks                  =  YO.Sequence . map encodeTaskNode

encodeTaskNode (Node task tasks) =
  YO.Mapping [((YO.toYamlScalar . yamlName) task, undefined)]

encodeTask (Command cmd test)  = YO.Sequence [argSequence cmd]
 where
  argSequence                =  YO.Sequence . map YO.toYamlScalar . args
encodeTask (Package name test) = YO.Mapping [(undefined,undefined)]


optionalTest task            =  Nothing


yamlName                    ::  Task -> ByteString
yamlName (Package name _)    =  name `snoc` '/'
yamlName (Command cmd _)     =  case cmd of
  CHOWN _ _                 ->  "CHOWN"
  CHMOD _ _                 ->  "CHMOD"
  RM _                      ->  "RM"
  CP _ _                    ->  "CP"
  LNs _ _                   ->  "LNs"
  TOUCH _                   ->  "TOUCH"
  MKDIR _                   ->  "MKDIR"
  USERADD _ _               ->  "USERADD"
  USERDEL _                 ->  "USERDEL"
  GROUPADD _ _              ->  "GROUPADD"
  GROUPDEL _                ->  "GROUPDEL"
  GPASSWDm _ _ _            ->  "GPASSWDm"

args                        ::  Command -> [ByteString]
args cmd                     =  case cmd of
  CHOWN p o                 ->  [enc p, chownStyle o]
  CHMOD p m                 ->  [enc p, modeSymbolic m]
  RM p                      ->  [enc p]
  CP p' p                   ->  [enc p', enc p]
  LNs p' p                  ->  [enc p', enc p]
  TOUCH p                   ->  [enc p]
  MKDIR p                   ->  [enc p]
  USERADD nick _            ->  [enc nick]
  USERDEL nick              ->  [enc nick]
  GROUPADD nick _           ->  [enc nick]
  GROUPDEL nick             ->  [enc nick]
  GPASSWDm gNick inU outU   ->  enc gNick : f '+' inU ++ f '-' outU
   where
    f char                   =  map (cons char . enc) . Set.toList

