
{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
 #-}

module System.TaskL where

import Data.String
import Control.Applicative
import Control.Monad.Identity

import Data.ByteString

import qualified System.TaskL.IdemShell as IdemShell


{-| The compiler input is this 'TaskGraph', a DAG of tasks. One may attach
    tests and dependencies to every node.

    The 'TaskGraph' is reduced to a sequence of operations in the 'Op' type.
    The schedule is such that each operation on a given system object -- for
    example, changing permissions or adding a user -- is executed but one
    time. Packages with overlapping operations are interleaved as necessary.

    TaskL can merge operations that overlap in their effect but do not
    conflict in order to follow the "only once" rule. For example, if one task
    specifies the user permissions and another the group permissions, these
    can be turned in to one operation that sets both. When conflicts between
    similar operations can not be resolved, the compiler will signal an error.

 -}
data TaskGraph               =  TaskGraph Task [IdemShell.Test] [TaskGraph]

{-| A task may be either a command or a package. Commands provide their own
    labels. 
 -}
data Task                    =  Command IdemShell.Command
                             |  Package ByteString

{-| The operations every backend must support.
 -}
data Op                      =  Enter ByteString
                             |  Leave ByteString
                             |  Check ByteString [IdemShell.Test]
                             |  Perform IdemShell.Command





