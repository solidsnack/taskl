{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
           , RecordWildCards
           , NamedFieldPuns
           , NoMonomorphismRestriction
  #-}

{-| Pretty printer for Bash. Not very pretty right now.
 -}

module System.TaskL.Bash.PrettyPrinter where

import qualified Data.List as List
import Data.Monoid
import Prelude hiding (concat, length, replicate)
import Data.Binary.Builder hiding (append)
import Data.ByteString.Char8
import Data.Word

import System.TaskL.Bash.Program


ops                         ::  Term -> [PPOp]
ops term                     =  case term of
  SimpleCommand cmd vals    ->  hang cmd ++ undefined ++ outdent
  Empty                     ->  word ": 'Do nothing.'"
  Bang t                    ->  hang "!" ++ ops t ++ outdent
  And t t'                  ->  ops t ++ word "&&" ++ nl ++ ops t'
  Or t t'                   ->  ops t ++ word "||" ++ nl ++ ops t'
  Pipe t t'                 ->  ops t ++ word "|"  ++ nl ++ ops t'
  Sequence t t'             ->  ops t              ++ nl ++ ops t'
  Background t t'           ->  ops t ++ word "&"  ++ nl ++ ops t'
  Group t                   ->  hang "{"  ++ ops t ++ word ";}" ++ outdent
  Subshell t                ->  hang "("  ++ ops t ++ word ")"  ++ outdent
  IfThen t t'               ->  hang "if" ++ ops t ++ outdent ++ nl
                            ++  inword "then"      ++ ops t'  ++ outword "fi"
  IfThenElse t t' t''       ->  hang "if" ++ ops t ++ outdent ++ nl
                            ++  inword "then"      ++ ops t'  ++ outdent
                            ++  inword "else"      ++ ops t'' ++ outword "fi"
  ForDoDone var vals t      ->  hang (concat ["for ", var, " in"])
                            ++  undefined
                            ++  outdent ++ nl
                            ++  inword "do" ++ ops t ++ outword "done"
  VarAssign var val         ->  wordcat [var, "=", val]
  DictDecl var pairs        ->  wordcat ["declare -A ", var, "=("] ++ nl
                            ++  List.concatMap arrayset pairs
                            ++  nl ++ word ")"
  DictUpdate var key val    ->  wordcat [var, "[", key, "]=", val]
  DictAssign var pairs      ->  wordcat [var, "=("] ++ nl
                            ++  List.concatMap arrayset pairs
                            ++  nl ++ word ")"
 where
  nl                         =  [Newline]
  hang bytes                 =  [Word bytes, Indent (cast (length bytes) + 1)]
  word bytes                 =  [Word bytes]
  wordcat                    =  word . concat
  backslash bytes            =  [Word "\\", Newline, Word bytes]
  outdent                    =  [Outdent]
  inword bytes               =  [Word bytes, Indent 2]
  outword bytes              =  [Outdent, Word bytes]
  arrayset (key, val)        =  [Word (concat ["[", key, "]=", val]), Newline]

{-| State of pretty printing -- string being built, indent levels, whether
    we've started a new line or not. 
 -}
data PPState                 =  PPState { indents :: [Word]
                                        , flag :: Bool
                                        , string :: Builder }

{-| Operations we can perform while pretty printing:

 *  Add n spaces to the indentation.

 *  Strip off a level of indentation.

 *  Move to the next line.

 *  Append a shell word to the current line.

 -}
data PPOp = Indent Word | Outdent | Word ByteString | Newline


{-| Apply an operation to a state. 
 -}
op                          ::  PPState -> PPOp -> PPState
op state@PPState{..} x       =  case x of
  Indent w                  ->  state {indents = w:indents}
  Outdent                   ->  state {indents = ht indents}
  Newline                   ->  state {flag = True}
  Word b                    ->  state {string = string `mappend` pad b}
 where
  pad b = fromByteString $ if flag then replicate dent ' ' `append` b
                                   else ' ' `cons` b
  dent                       =  cast (sum indents)
  ht                         =  List.head . List.tails

{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  Line break algorithm for simple commands and arguments of for loops:

    IF the width of the current line with the given word added is greater
       than 79 columns
    THEN
      IF moving the word to the following line causes the line to be shorter
      THEN
        do it
      ELSE
        add the current word to the current line
      DONE
    ELSE
      add the current word to the current line
    DONE

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}

cast                         =  fromIntegral

