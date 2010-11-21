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

import Text.ShellEscape

import System.TaskL.Op
import System.TaskL.Task
import System.TaskL.Bash.Program


arrayKeys                   ::  [Op] -> [Bash]
arrayKeys                    =  map bash . sort . nub . map labelTask


enabledArray                ::  [Bash] -> ArrayDecl
enabledArray                 =  undefined


main                        ::  [Op] -> Term
main                         =  undefined


code                        ::  Op -> Program
code op@(Op (code, (_, t))   =  case code of
  Enter                     ->  msg op
  Check                     ->  msg op
  Enable                    ->  msg op
  Exec                      ->  msg op
  Leave                     ->  msg op


msg op@(Op (code, (_, t))    =  SimpleCommand (ARGV [f, labelTask op])
 where
  f                          =  case code of
    Enter                   ->  SimpleCommand (ARGV ["msg_enter",  label])
    Check                   ->  SimpleCommand (ARGV ["msg_check",  label])
    Enable                  ->  SimpleCommand (ARGV ["msg_enable", label])
    Exec                    ->  SimpleCommand (ARGV ["msg_exec",   label])
    Leave                   ->  SimpleCommand (ARGV ["msg_leave",  label])


data ArrayDecl


