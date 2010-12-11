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

import Data.List (sort, nub)
import Data.Monoid

import qualified Text.ShellEscape as Esc

import System.TaskL.IdemShell (essentialTest)
import System.TaskL.Op
import System.TaskL.Task
import System.TaskL.Bash.Program
import System.TaskL.Bash.Codegen.Utils
import System.TaskL.Bash.Codegen.IdemShell


arrayKeys                   ::  [Op] -> [Esc.Bash]
arrayKeys                    =  map Esc.bash . sort . nub . map labelTask


main                        ::  [Op] -> Term
main                         =  undefined


code                        ::  Op -> Term
code op@(Op (code, (_, t)))  =  case code of
  Enter                     ->  m
  Check                     ->  m `And` checkCode (task t)
  Enable                    ->  m `And` enableCode (depLabels t)
  Exec                      ->  m `And` execCode (task t)
  Leave                     ->  m
 where
  m                          =  msg op
  depLabels                  =  map label . dependencies


msg op@(Op (code, (_, t)))   =  SimpleCommand (ARGV [s, labelTask op])
 where
  s                          =  case code of
    Enter                   ->  "taskl_enter"
    Check                   ->  "taskl_check"
    Enable                  ->  "taskl_enable"
    Exec                    ->  "taskl_exec"
    Leave                   ->  "taskl_leave"


checkCode                   ::  Task -> Term
checkCode task               =  codeGen test `And` checkSet
 where
  test                       =  case task of
    Command c extraTest     ->  extraTest `mappend` essentialTest c
    Package _ extraTest     ->  extraTest
  checkSet                   =  DictUpdate "taskl_checks" (label task) "true"


execCode                    ::  Task -> Term
execCode task                =  case task of
  Command c extraTest       ->  codeGen c
  Package _ extraTest       ->  Empty


enableCode                  ::  [ByteString] -> Term
enableCode depLabels         =  ForDoDone "task" depLabels
                                  (DictUpdate "taskl_enabled" "$task" "true")


