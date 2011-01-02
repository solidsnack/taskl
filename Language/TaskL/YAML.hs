
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

instance YShow Command where
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
  yshow test                 =  case test of
    LSo _ _                 ->  m "LSo" undefined
    LSm _ _                 ->  m "LSm" undefined
    DASHe _                 ->  m "DASHe" undefined
    DASH_ _ _               ->  m "DASH_" undefined
    DIFFq _ _               ->  m "DIFFq" undefined
    LSl _ _                 ->  m "LSl" undefined
    GETENTu _               ->  m "GETENTu" undefined
    GETENTg _               ->  m "GETENTg" undefined
    GROUPS _ _              ->  m "GROUPS" undefined
    Not _                   ->  m "NOT" undefined
    And _ _                 ->  m "AND" undefined
    Or _ _                  ->  m "OR" undefined
    TRUE                    ->  scalarO "TRUE"
    FALSE                   ->  scalarO "FALSE"
   where
    argsYAML test            =  case test of
      LSo p o               ->  [enc p,   chownStyle o]
      LSm p m               ->  [enc p,   modeSymbolic m]
      DASHe p               ->  [enc p]
      DASH_ node p          ->  [nodeTest node, enc p]
      DIFFq p' p            ->  [enc p',  enc p]
      LSl p' p              ->  [enc p',  enc p]
      GETENTu u             ->  [enc u]
      GETENTg g             ->  [enc g]
      GROUPS u g            ->  [enc u,  enc g]
      Not t                 ->  []
      And t t'              ->  []
      Or t t'               ->  []
      TRUE                  ->  []
      FALSE                 ->  []

scalarO                      =  YO.Scalar . YO.toYamlScalar

scalarY                      =  YO.toYamlScalar

m k v                        =  YO.Mapping [(scalarY k, v)]

