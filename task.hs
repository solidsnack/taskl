{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
           , ParallelListComp
  #-}


import Data.Tree

import System.TaskL.IdemShell
import System.TaskL.Combination
import System.TaskL.IndexForest
import System.TaskL.Task
import System.TaskL.Op
import System.TaskL.Schedule


tasks0 =
  [ Node (Command (LNs "/foo" "/bar") []) []
  ]

tasks1 =
  [ Node (Command (LNs "/foo" "/bar") []) []
  , Node (Command (TOUCH "/x/a") []) [Node (Command (MKDIR "/x") []) []]
  , Node (Command (TOUCH "/x/b") []) [Node (Command (MKDIR "/x") []) []]
  ]


