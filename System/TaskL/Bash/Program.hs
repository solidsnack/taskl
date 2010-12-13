{-# LANGUAGE EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
  #-}

module System.TaskL.Bash.Program where

import Prelude hiding (all)
import Data.Char
import Data.String
import Data.Maybe
import Data.ByteString.Char8

import qualified Text.ShellEscape as Esc


{-| Terms that can be combined with one another.
@
  <term>                     =  <simple command>
                             |  ! <term>
                             |  <term> && <term>
                             |  <term> || <term>
                             |  <term> | <term>
                             |  <term> ; <term>
                             |  <term> & <term>
                             |  { <term> ;}
                             |  '(' <term> ')'
                             |  if <term> then <term> else <term>
                             |  if <term> then <term>
                             |  <name>=<text>
                             |  declare -A <name>='('([<name>]=<text>)*')'
                             |  <name>[<name>]=<text>
@
 -}
data Term                    =  SimpleCommand ARGV
                             |  Empty
                             |  Bang Term
                             |  And Term Term
                             |  Or Term Term
                             |  Pipe Term Term
                             |  Sequence Term Term
                             |  Background Term Term
                             |  Group Term
                             |  Subshell Term
                             |  IfThen Term Term
                             |  IfThenElse Term Term Term
                             |  ForDoDone ByteString [ByteString] Term
                             |  VarAssign ByteString ByteString
                             |  DictDecl ByteString [(ByteString, ByteString)]
                             |  DictUpdate ByteString ByteString ByteString
                             |  DictAssign ByteString
                                           [(ByteString, ByteString)]
deriving instance Eq Term
deriving instance Ord Term
deriving instance Show Term


newtype ARGV                 =  ARGV [ByteString]
deriving instance Eq ARGV
deriving instance Ord ARGV
deriving instance Show ARGV


cmd                         ::  [ByteString] -> Term
cmd                          =  SimpleCommand . ARGV


-- Unused.
data Expression              =  Literal Esc.Bash
                             |  DeReference Identifier
-- Ignore                    |  Exec Term


newtype Identifier           =  Identifier ByteString
deriving instance Eq Identifier
deriving instance Ord Identifier
deriving instance Show Identifier
instance IsString Identifier where
  fromString                 =  fromJust . identifier . fromString

identifier                  ::  ByteString -> Maybe Identifier
identifier bytes             =  do
  (c, bytes')               <-  uncons bytes
  if okayHead c && all okayTail bytes'
    then  Just (Identifier bytes)
    else  Nothing
 where
  okayTail c                 =  (isAlphaNum c || c == '_') && isAscii c
  okayHead c                 =  (isAlpha c || c == '_') && isAscii c

