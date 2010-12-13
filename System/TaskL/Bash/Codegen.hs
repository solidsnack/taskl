{-# LANGUAGE EmptyDataDecls
           , StandaloneDeriving
           , OverloadedStrings
  #-}

{-| Code generation from a description of tasks.

    A static preamble is included in all Bash scripts. Following this
    preamble, we generate an array for holding the enabled/disabled state of
    every task. Below that we place the code for performing the tasks.

 -}

module System.TaskL.Bash.Codegen where

import Data.Ord
import Control.Arrow (first, second, (&&&))
import Data.List (sortBy, nub)
import Data.Monoid

import Data.ByteString.Char8 (ByteString)
import qualified Text.ShellEscape as Esc

import System.TaskL.IdemShell (essentialTest)
import System.TaskL.Op
import System.TaskL.Task
import System.TaskL.Bash.Program
import System.TaskL.Bash.Codegen.IdemShell


labelAndSort                ::  [Op] -> [(ByteString, Task)]
labelAndSort                 =  nub . map (first (Esc.bytes . Esc.bash))
                             .  sortBy (comparing fst)
                             .  map (labelTask &&& task)


stateArrays                 ::  [Op] -> Term
stateArrays ops              =  Sequence (DictAssign "taskl_enabled" falses)
                                         (DictAssign "taskl_checks" checks)
 where
  falses                     =  map (second (const "false")) labelled
  checks                     =  map (second checkOK) labelled
  labelled                   =  labelAndSort ops
  checkOK (Package _ t)
    | t == mempty            =  "true"
    | otherwise              =  "false"
  checkOK _                  =  "false"


code                        ::  Op -> Term
code op@(Op (code, _))       =  case code of
  Enter                     ->  msg
  Check                     ->  msg `And` checkCode (task op)
  Enable                    ->  msg `And` enableCode (depLabels op)
  Exec                      ->  msg `And` execCode (task op)
  Leave                     ->  msg
 where
  depLabels                  =  map label . dependencies
  msg                        =  SimpleCommand . ARGV . (:[labelTask op]) $
    case code of
      Enter                 ->  "taskl_enter"
      Check                 ->  "taskl_check"
      Enable                ->  "taskl_enable"
      Exec                  ->  "taskl_exec"
      Leave                 ->  "taskl_leave"


checkCode                   ::  Task -> Term
checkCode task               =  codeGen test `And` checkSet
 where
  test                       =  case task of
    Command c extraTest     ->  extraTest `mappend` essentialTest c
    Package _ extraTest     ->  extraTest
  checkSet                   =  DictUpdate "taskl_checks" (label task) "true"


execCode                    ::  Task -> Term
execCode task                =  case task of
  Command c _               ->  codeGen c
  Package _ _               ->  Empty


enableCode                  ::  [ByteString] -> Term
enableCode depLabels         =  ForDoDone "task" depLabels
                                  (DictUpdate "taskl_enabled" "$task" "true")


