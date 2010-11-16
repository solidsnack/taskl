{-# LANGUAGE StandaloneDeriving
           , TupleSections
  #-}

{-| The 'Op' and 'OpCode' datatypes describe the five operations that a
    Task\\L program executes for each task.
 -}

module System.TaskL.Op where

import Data.Ord
import Data.Tree
import Data.ByteString (ByteString, append)

import System.TaskL.IndexForest
import System.TaskL.Task


ops                         ::  Tree (Index, Task) -> [Op]
ops t                        =  map (Op . (,t)) $ case t of
  Node (_, Package _ _) _   ->  [Enter, Check, Enable, Leave]
  Node (_, Command _ _) _   ->  [Enter, Check, Enable, Exec, Leave]


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


Node _ f `dependsOn` Node t _ = (any (== snd t) . concatMap rawTasks) f


t `sharesDeps` t'            =  any (`elem` rawTasks t) (rawTasks t')


rawTasks                   =  flatten . fmap snd


display                     ::  Op -> ByteString
display (Op (code, t))       =  lead `append` (label . snd . rootLabel) t
 where
  lead                       =  case code of
    Enter                   ->  " >> "
    Leave                   ->  " << "
    Check                   ->  " ** "
    Enable                  ->  " @@ "
    Exec                    ->  " ++ "

