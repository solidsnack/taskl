{-# LANGUAGE EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
  #-}

module System.TaskL.Bash.Program where

import Data.ByteString


{-| Terms that can be combined with one another.
@
  <term>                     =  <simple command>
                             |  ! <term>
                             |  <term> && <term>
                             |  <term> || <term>
                             |  <term> | <term>
                             |  <term> ; <term>
                             |  <term> & <term>
                             |  { <term>+ ;}
                             |  '(' <term>+ ')'
                             |  if <term>+ then <term>+ else <term>+
                             |  if <term>+ then <term>+
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
                             |  Group [Term]
                             |  Subshell [Term]
                             |  IfThen [Term] [Term]
                             |  IfThenElse [Term] [Term] [Term]
                             |  VarAssign ByteString ByteString
                             |  DictDecl ByteString [(ByteString, ByteString)]
                             |  DictUpdate ByteString ByteString ByteString
deriving instance Eq Term
deriving instance Ord Term
deriving instance Show Term


newtype ARGV                 =  ARGV [ByteString]
deriving instance Eq ARGV
deriving instance Ord ARGV
deriving instance Show ARGV


cmd                         ::  [ByteString] -> Term
cmd                          =  SimpleCommand . ARGV


