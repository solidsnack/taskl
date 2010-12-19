module Language.TaskL.Codegen.Utils where

import Data.ByteString
import qualified Text.ShellEscape as Esc

import Language.TaskL.EncDec


escEnc                      ::  (EncDec t) => t -> ByteString
escEnc                       =  esc . enc


esc                         ::  ByteString -> ByteString
esc                          =  Esc.bytes . Esc.bash


