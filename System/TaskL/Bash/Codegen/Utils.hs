{-# LANGUAGE OverloadedStrings
  #-}

module System.TaskL.Bash.Codegen.Utils where

import Data.ByteString
import qualified Text.ShellEscape as Esc

import Data.ByteString.EncDec
import System.TaskL.IdemShell
import System.TaskL.IdemShell.Path
import System.TaskL.Bash.Program


testFS                      ::  ByteString -> Path -> Term
testFS t p                   =  cmd ["[", t, escEnc p, "]"]


readLink p                   =  "`readlink " `append` escEnc p `append` "`"


escEnc                       =  Esc.bytes . Esc.bash . enc




