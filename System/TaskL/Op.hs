

module System.TaskL.Op where

import Data.Tree

import System.TaskL.IndexForest
import System.TaskL.Task


{-| A backend supports these operations.
 -}
data Op = Enter  (Tree (Index, Task))        -- ^ Notify of task start.
        | Leave  (Tree (Index, Task))        -- ^ Notify of task completion.
        | Enable (Tree (Index, Task))        -- ^ Enable immediate subtasks.
        | Check  (Tree (Index, Task))        -- ^ Perform checks for task.
        | Exec   (Tree (Index, Task))        -- ^ Apply configuration.

