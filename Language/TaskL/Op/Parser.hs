{-# LANGUAGE MultiParamTypeClasses
           , OverloadedStrings
  #-}

module Language.TaskL.Op.Parser where

import Data.Tree

import Data.Number.Natural
import Text.ParserCombinators.UU

import Language.TaskL.Op
import Language.TaskL.Task
import Language.TaskL.IndexForest


type OpParser t              =  P (Str Op Natural) t


pCode                       ::  OpCode -> OpParser Op
pCode code                   =  pSym (test, s, auto)
 where
  test (Op (code', _))       =  code == code'
  s                          =  show code ++ ", _"
  auto                       =  Op (code, (Node (Index [], Package "" [])[]))


instance IsLocationUpdatedBy Natural Op where
  advance p _                =  p + 1

