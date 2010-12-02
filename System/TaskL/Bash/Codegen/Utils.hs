{-# LANGUAGE OverloadedStrings
  #-}

module System.TaskL.Bash.Codegen.Utils where

import qualified Data.List as List
import Data.ByteString
import qualified Text.ShellEscape as Esc

import Data.ByteString.EncDec
import System.TaskL.IdemShell
import System.TaskL.IdemShell.Path
import System.TaskL.Bash.Program (cmd)
import qualified System.TaskL.Bash.Program as Program


testFS                      ::  ByteString -> Path -> Program.Term
testFS t p                   =  cmd ["[", t, escEnc p, "]"]


getent                      ::  GettableEnt -> Program.Term
getent (User nick)           =  cmd ["getent", "passwd", escEnc nick]
getent (Group nick)          =  cmd ["getent", "group", escEnc nick]


escEnc                      ::  (EncDec t) => t -> ByteString
escEnc                       =  esc . enc


esc                         ::  ByteString -> ByteString
esc                          =  Esc.bytes . Esc.bash


pipeline                    ::  [Program.Term] -> Program.Term
pipeline []                  =  cmd ["true"]
pipeline [term]              =  term
pipeline (a:b:terms)         =  List.foldr Program.Pipe a (b:terms)


