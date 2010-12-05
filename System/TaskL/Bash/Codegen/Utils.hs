module System.TaskL.Bash.Codegen.Utils where

import Data.ByteString
import qualified Text.ShellEscape as Esc

import Data.ByteString.EncDec


escEnc                      ::  (EncDec t) => t -> ByteString
escEnc                       =  esc . enc


esc                         ::  ByteString -> ByteString
esc                          =  Esc.bytes . Esc.bash


