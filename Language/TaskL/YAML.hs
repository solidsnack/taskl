
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


class YShow t where
  yshow                     ::  t -> YO.YamlObject

instance (EncDec t) => YShow t where
  yshow                      =  scalarO . enc

instance YShow Command where
  yshow cmd                  =  case cmd of
    CHOWN p o               ->  "CHOWN" `kvO` yshowSeq [p, o]
    CHMOD p m               ->  "CHMOD" `kvO` yshowSeq [p, modeSymbolic m]
    RM p                    ->  "RM" `kvO` yshow p
    CP p' p                 ->  "CP" `kvO` yshowSeq [p', p]
    LNs p' p                ->  "LNs" `kvO` yshowSeq [p', p]
    TOUCH p                 ->  "TOUCH" `kvO` yshow p
    MKDIR p                 ->  "MKDIR" `kvO` yshow p
    USERADD nick _          ->  "USERADD" `kvO` nick
    USERDEL nick            ->  "USERDEL" `kvO` nick
    GROUPADD nick _         ->  "GROUPADD" `kvO` nick
    GROUPDEL nick           ->  "GROUPDEL" `kvO` nick
    GPASSWDm nick iU oU     ->  "GPASSWDm" `kvO` enc nick : f '+' iU ++ f '-' oU
     where
      f char                 =  map (cons char . enc) . Set.toList
  yshow cmd                  =  YO.Mapping [(name cmd, argSequence cmd)]
   where
    argSequence              =  YO.Sequence . map scalarO . args
    name                     =  scalarY . yamlName
    yamlName                ::  Command -> ByteString
    yamlName cmd             =  case cmd of
      CHOWN _ _             ->  "CHOWN"
      CHMOD _ _             ->  "CHMOD"
      RM _                  ->  "RM"
      CP _ _                ->  "CP"
      LNs _ _               ->  "LNs"
      TOUCH _               ->  "TOUCH"
      MKDIR _               ->  "MKDIR"
      USERADD _ _           ->  "USERADD"
      USERDEL _             ->  "USERDEL"
      GROUPADD _ _          ->  "GROUPADD"
      GROUPDEL _            ->  "GROUPDEL"
      GPASSWDm _ _ _        ->  "GPASSWDm"
    args cmd                 =  case cmd of
      CHOWN p o             ->  [enc p, chownStyle o]
      CHMOD p m             ->  [enc p, modeSymbolic m]
      RM p                  ->  [enc p]
      CP p' p               ->  [enc p', enc p]
      LNs p' p              ->  [enc p', enc p]
      TOUCH p               ->  [enc p]
      MKDIR p               ->  [enc p]
      USERADD nick _        ->  [enc nick]
      USERDEL nick          ->  [enc nick]
      GROUPADD nick _       ->  [enc nick]
      GROUPDEL nick         ->  [enc nick]
      GPASSWDm nick iU oU   ->  enc nick : f '+' iU ++ f '-' oU
       where
        f char               =  map (cons char . enc) . Set.toList

instance YShow Test where
  yshow test                 =  case collapse test of
    LSo p o                 ->  "LSo" `kvO` yshowSeq [p, o]
    LSm p o                 ->  "LSm" `kvO` yshowSeq [p, modeSymbolic m]
    DASHe p                 ->  "DASHe" `kvO` yshow p
    DASH_ node p            ->  "DASH_" `kvO` yshowSeq [nodeTest node, p]
    DIFFq p p'              ->  "DIFFq" `kvO` yshowSeq [p, p']
    LSl p p'                ->  "LSl" `kvO` yshowSeq [p, p']
    GETENTu u               ->  "GETENTu" `kvO` yshow u
    GETENTg g               ->  "GETENTg" `kvO` yshow g
    GROUPS u g              ->  "GROUPS" `kvO` yshowSeq [u, g]
    Not t                   ->  "NOT" `kvO` yshowSeq [t]
    And t t'                ->  "AND" `kvO` yshowSeq [t, t']
    Or t t'                 ->  "OR" `kvO` yshowSeq [t, t']
    TRUE                    ->  scalarO "TRUE"
    FALSE                   ->  scalarO "FALSE"

scalarO                      =  YO.Scalar . YO.toYamlScalar

scalarY                      =  YO.toYamlScalar

kvO k v                      =  YO.Mapping [(scalarY k, v)]

seqO                         =  YO.Sequence . map scalarO

yshowSeq                     =  YO.Sequence . map yshow

