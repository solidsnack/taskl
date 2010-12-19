{-# LANGUAGE EmptyDataDecls
           , StandaloneDeriving
           , OverloadedStrings
  #-}

{-| Code generation from a description of tasks.

    A static preamble is included in all Bash scripts. Following this
    preamble, we generate an array for holding the enabled/disabled state of
    every task. Below that we place the code for performing the tasks.

 -}

module Language.TaskL.Codegen where

import Data.Ord
import Control.Arrow (first, second, (&&&))
import Data.List (sortBy, nub, foldl')
import Data.Monoid

import Data.ByteString.Char8 (ByteString)
import qualified Text.ShellEscape as Esc

import Language.TaskL.IdemShell (essentialTest)
import Language.TaskL.Op
import Language.TaskL.Task
import Language.TaskL.Bash.Program
import Language.TaskL.Codegen.IdemShell
import Language.TaskL.Codegen.Utils


code                        ::  [Task] -> [Op] -> (Term, Term)
code tasks ops               =  ( stateArrays tasks ops
                                , (fold . map codeForOp) ops )
 where
  fold [   ]                 =  Empty
  fold (h:t)                 =  foldl' Sequence h t


labelAndSort                ::  [Op] -> [(ByteString, Task)]
labelAndSort                 =  nub . map (first esc)
                             .  sortBy (comparing fst)
                             .  map (labelTask &&& task)


stateArrays                 ::  [Task] -> [Op] -> Term
stateArrays tasks ops        =  Sequence (DictAssign "taskl_enabled" enables)
                                         (DictAssign "taskl_checks" checks)
 where
  enables                    =  map (second topLevel) labelled
  checks                     =  map (second checkOK) labelled
  labelled                   =  labelAndSort ops
  topLevel task              =  if label task `elem` topLevelLabels
                                  then  "true"
                                  else  "false"
  topLevelLabels             =  map label tasks
  checkOK (Package _ t)
    | t == mempty            =  "true"
    | otherwise              =  "false"
  checkOK _                  =  "false"


codeForOp                   ::  Op -> Term
codeForOp op@(Op (code, _))  =  case code of
  Enter                     ->  msg
  Check                     ->  IfThen msg (checkCode (task op))
  Enable                    ->  IfThen msg (enableCode (depLabels op))
  Exec                      ->  IfThen msg (execCode (task op))
  Leave                     ->  msg
 where
  depLabels                  =  map label . dependencies
  msg                        =  flip SimpleCommand [esc $ labelTask op] $
    case code of
      Enter                 ->  "taskl_enter"
      Check                 ->  "taskl_check"
      Enable                ->  "taskl_enable"
      Exec                  ->  "taskl_exec"
      Leave                 ->  "taskl_leave"


checkCode                   ::  Task -> Term
checkCode task               =  IfThen (codeGen test) checkSet
 where
  test                       =  case task of
    Command c extraTest     ->  extraTest `mappend` essentialTest c
    Package _ extraTest     ->  extraTest
  checkSet                   =  DictUpdate "taskl_checks"
                                           (esc $ label task) "true"


execCode                    ::  Task -> Term
execCode task                =  case task of
  Command c _               ->  codeGen c
  Package _ _               ->  Empty


enableCode                  ::  [ByteString] -> Term
enableCode depLabels         =  ForDoDone "task" (map esc depLabels)
                                  (DictUpdate "taskl_enabled"
                                              "\"$task\"" "true")

