{-# LANGUAGE StandaloneDeriving
           , OverloadedStrings
           , TupleSections
  #-}

{-| The 'Op' and 'OpCode' datatypes describe the five operations that a
    Task\\L program executes for each task.
 -}

module System.TaskL.Op
  ( Op(..)
  , OpCode(..)
  , ops
  , dependsOn
  , sharesDeps
  , labelTask
  , display
  ) where

import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Tree
import Data.ByteString (ByteString, append)

import System.TaskL.IndexForest
import System.TaskL.Task


ops                         ::  Tree (Index, Task) -> [Op]
ops t@(Node (_, task) sub)   =  (map (Op . (,t)) .  catMaybes)
  [Just Enter, Check !? test, Enable !? sub, exec, Just Leave]
 where
  (test, exec)               =  case task of Command _ x    ->  (x, Just Exec)
                                             Package _ x    ->  (x, Nothing)
  k !? x                     =  if x == mempty then Nothing else Just k

{-| A backend supports these operations.

    The 'Ord' instance for 'Op' encodes the rule for ordering of operations:

* 'Enter', 'Check' and 'Enable' for a dependent go before that of a
  dependency.

* 'Exec' and 'Leave' for a dependent go after that of a dependency.

 -}
newtype Op                   =  Op (OpCode, (Tree (Index, Task)))
deriving instance Eq Op
deriving instance Show Op
instance Ord Op where
  Op (c, t) `compare` Op (c', t')
    | t `dependsOn` t'       =  case c of Enter  ->  LT
                                          Check  ->  LT
                                          Enable ->  LT
                                          Exec   ->  GT
                                          Leave  ->  GT
    | t' `dependsOn` t       =  Op (c', t') `compare` Op (c, t)
    | t == t'                =  compare c c'
    | t `sharesDeps` t'      =  if c == c' then lexicalOrder else compare c c'
    | otherwise              =  lexicalOrder
   where
    lexicalOrder             =  comparing (fst . rootLabel) t t'


data OpCode  =  Enter  -- ^ Publish notification that a task is starting.
             |  Check  -- ^ Test to see if task has already been performed.
             |  Enable -- ^ Flag direct dependencies for execution.
             |  Exec   -- ^ Run task if the check did not succeed.
             |  Leave  -- ^ Publish notification that a task is complete.
deriving instance Eq OpCode
deriving instance Show OpCode
instance Ord OpCode where
  compare Enter  Enter       =  EQ
  compare Enter  _           =  LT
  compare Check  Enter       =  GT
  compare Check  Check       =  EQ
  compare Check  _           =  LT
  compare Enable Enter       =  GT
  compare Enable Check       =  GT
  compare Enable Enable      =  EQ
  compare Enable _           =  LT
  compare Exec   Exec        =  EQ
  compare Exec   Leave       =  LT
  compare Exec   _           =  GT
  compare Leave  Leave       =  EQ
  compare Leave  _           =  GT


-- TODO  Destroy all this and move to using a topological sort for scheduling.

dependsOn :: Tree (Index, Task) -> Tree (Index, Task) -> Bool
Node _ f `dependsOn` t       =  (any (== lbR t) . concatMap lbF) f


sharesDeps :: Tree (Index, Task) -> Tree (Index, Task) -> Bool
t `sharesDeps` t'            =  any (`elem` lbF t) (lbF t')


lb                          ::  (Index, Task) -> ByteString
lb                           =  label . snd


lbR                         ::  Tree (Index, Task) -> ByteString
lbR                          =  lb . rootLabel


lbF                         ::  Tree (Index, Task) -> [ByteString]
lbF                          =  map lb . flatten


display                     ::  Op -> ByteString
display (Op (code, t))       =  lead `append` lbR t
 where
  lead                       =  case code of
    Enter                   ->  " >> "
    Leave                   ->  " << "
    Check                   ->  " ** "
    Enable                  ->  " ++ "
    Exec                    ->  " @@ "


labelTask                   ::  Op -> ByteString
labelTask (Op (code, t))     =  lbR t


