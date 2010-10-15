
{-# LANGUAGE OverloadedStrings
  #-}

module System.TaskL.BashCodegen where

import Prelude
import qualified Data.List as List
import Data.String
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Word

import Data.Text (Text) 
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Text.ShellEscape as Esc


data Test
data Command
data Statement               =  CallCode ByteString
                             |  CallCheck ByteString
                             |  CommandCode Command
                             |  TestCode Test
                             |  If Statement [Statement]
                                             [Statement]
                             |  Unless Statement [Statement]
                                                 [Statement]

caseFunction :: ByteString -> [(ByteString, [Statement])] -> [LogicalLine]
caseFunction name cases      =  function name (caseStatement "\"$1\"" clauses)
 where
  clauses                    =  concatMap (uncurry codeForLabel) cases
  codeForLabel label statements = caseClause label (genLLs statements)

genLLs                       =  undefined

function name body           =  wrapDent (basicLL ["function ", name, " {"])
                                         body
                                         "}"

caseStatement var body       =  wrapDent (basicLL ["case ", var, " of"])
                                         body
                                         "esac"

basicLL strings = LL 0 [UnsafeLiteral (ByteString.concat strings)]


caseClause s body            =  wrapDent (LL 0 [StringLiteral (Esc.bash s)])
                                         body'
                                         "  ;;"
 where
  body'                      =  case body of [ ] -> ["  : # Do nothing."]
                                             _:_ -> fmap (indent 2) body

data LogicalLine             =  LL Word [Element]
instance IsString LogicalLine where
  fromString s               =  LL ((fromIntegral . List.length) leadingSpaces)
                                   [(fromString other)]
   where
    (leadingSpaces, other)   =  List.break (== ' ') s

indent                      ::  Word -> LogicalLine -> LogicalLine
indent i' (LL i text)        =  LL (i + i') text

wrapDent :: LogicalLine -> [LogicalLine] -> LogicalLine -> [LogicalLine]
wrapDent a bs c              =  a : (fmap (indent 2) bs) ++ [c]

data Element                 =  StringLiteral Esc.Bash
                             |  IntLiteral Integer
                             |  UnsafeLiteral ByteString
instance IsString Element where
  fromString                 =  UnsafeLiteral . Text.encodeUtf8 . Text.pack

