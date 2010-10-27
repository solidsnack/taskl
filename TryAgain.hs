
module TryAgain where

import Data.Tree
import Data.String
import Control.Applicative
import Control.Monad.Identity

import Data.ByteString
import Data.Number.Natural

import qualified System.TaskL.IdemShell as IdemShell




schedule                     =  sort . opCodes . merge . label




{-| A task may be either a command or a named package. Commands provide their
    own labels. Every 'Task' may attach tests to restrict the circumstances
    under which a command is run.
 -}
data Task                    =  Command IdemShell.Command [IdemShell.Test]
                             |  Package ByteString [IdemShell.Test]


{-| Label each task with its path from the root. 
 -}
label                       ::  Tree Task -> [([Natural], TreeTask)]


{-| Merge like subtrees to produce DAG nodes for fail in the attempt.
 -}
merge :: [([Natural], Tree Task)] -> Either Err [([[Natural]], Tree Task)]


data Op                      =  Enter LabelledSubtree
                             -- ^ Notify on entry to this subtree.
                             |  Leave LabelledSubtree
                             -- ^ Notify on exit from this subtree.
                             |  Enable LabelledSubtree
                             -- ^ Enable entry of this subtree.
                             |  Check LabelledSubtree
                             -- ^ Check tests for the root of this subtree.
                             |  Exec LabelledSubtree
                             -- ^ Perform actions for the root of this subtree.
instance Ord Op where
  Enter  (paths, tree) `compare` Enter  (paths', tree') = undefined
  Enter  (paths, tree) `compare` Leave  (paths', tree') = undefined
  Enter  (paths, tree) `compare` Enable (paths', tree') = undefined
  Enter  (paths, tree) `compare` Check  (paths', tree') = undefined
  Enter  (paths, tree) `compare` Exec   (paths', tree') = undefined
  Leave  (paths, tree) `compare` Enter  (paths', tree') = undefined
  Leave  (paths, tree) `compare` Leave  (paths', tree') = undefined
  Leave  (paths, tree) `compare` Enable (paths', tree') = undefined
  Leave  (paths, tree) `compare` Check  (paths', tree') = undefined
  Leave  (paths, tree) `compare` Exec   (paths', tree') = undefined
  Enable (paths, tree) `compare` Enter  (paths', tree') = undefined
  Enable (paths, tree) `compare` Leave  (paths', tree') = undefined
  Enable (paths, tree) `compare` Enable (paths', tree') = undefined
  Enable (paths, tree) `compare` Check  (paths', tree') = undefined
  Enable (paths, tree) `compare` Exec   (paths', tree') = undefined
  Check  (paths, tree) `compare` Enter  (paths', tree') = undefined
  Check  (paths, tree) `compare` Leave  (paths', tree') = undefined
  Check  (paths, tree) `compare` Enable (paths', tree') = undefined
  Check  (paths, tree) `compare` Check  (paths', tree') = undefined
  Check  (paths, tree) `compare` Exec   (paths', tree') = undefined
  Exec   (paths, tree) `compare` Enter  (paths', tree') = undefined
  Exec   (paths, tree) `compare` Leave  (paths', tree') = undefined
  Exec   (paths, tree) `compare` Enable (paths', tree') = undefined
  Exec   (paths, tree) `compare` Check  (paths', tree') = undefined
  Exec   (paths, tree) `compare` Exec   (paths', tree') = undefined

