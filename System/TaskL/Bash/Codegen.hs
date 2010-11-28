{-# LANGUAGE EmptyDataDecls
           , StandaloneDeriving
  #-}

{-| Code generation from a description of tasks.

    A static preamble is included in all Bash scripts. Following this
    preamble, we generate an array for holding the enabled/disabled state of
    every task. Below that we place the code for performing the tasks.

 -}

module System.TaskL.Bash.Codegen where

import Data.List (sort, nub)

import qualified Text.ShellEscape as Esc

import System.TaskL.IdemShell
import System.TaskL.Op
import System.TaskL.Task
import System.TaskL.Bash.Program


arrayKeys                   ::  [Op] -> [Esc.Bash]
arrayKeys                    =  map bash . sort . nub . map labelTask


main                        ::  [Op] -> Term
main                         =  undefined


code                        ::  Op -> Term
code op@(Op (code, (_, t))   =  case code of
  Enter                     ->  msg op
  Check                     ->  msg op `And` foldr
  Enable                    ->  msg op
  Exec                      ->  msg op
  Leave                     ->  msg op


msg op@(Op (code, (_, t))    =  Bash.SimpleCommand (ARGV [f, labelTask op])
 where
  f                          =  case code of
    Enter                   ->  SimpleCommand (ARGV ["msg_enter",  label])
    Check                   ->  SimpleCommand (ARGV ["msg_check",  label])
    Enable                  ->  SimpleCommand (ARGV ["msg_enable", label])
    Exec                    ->  SimpleCommand (ARGV ["msg_exec",   label])
    Leave                   ->  SimpleCommand (ARGV ["msg_leave",  label])


checkCode                   ::  Task -> Term
checkCode task = checkSet False `Sequence` foldr And (checkSet True) testCode
 where
  tests                      =  case task of
    Command c l             ->  essentialTests c ++ l
    Package _ l             ->  l
  testCode                   =  map codeGen tests
  checkSet b = VarAssign "check_state" (if b then "true" else "false")


