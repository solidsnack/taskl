{-# LANGUAGE OverloadedStrings
           , ParallelListComp
  #-}


import System.IO
import Data.Tree
import Data.Monoid
import qualified Data.Binary.Builder
import qualified Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8

import Data.Digest.Pure.SHA

import Language.TaskL.IdemShell
import Language.TaskL.Combination
import Language.TaskL.IndexForest
import Language.TaskL.Task
import Language.TaskL.Op
import Language.TaskL.Schedule
import Language.TaskL
import qualified Language.TaskL as TaskL


tasks0 =
  [ Node (Command (LNs "/foo" "/bar") mempty) []
  ]

tasks1 =
  [ Node (Command (LNs "/foo" "/bar") mempty) []
  , Node (Command (TOUCH "/x/a") mempty)
         [Node (Command (MKDIR "/x") mempty) []]
  , Node (Command (TOUCH "/x/b") mempty)
         [Node (Command (MKDIR "/x") mempty) []]
  ]

tasks2 =
  [ Node (Command (TOUCH "/q/a") mempty)
      [ Node (Command (MKDIR "/q") mempty) [] ]
  , Node (Command (TOUCH "/q/b") mempty)
      [ Node (Command (MKDIR "/q") mempty) []
      , Node (Command (MKDIR "/q/p") mempty) [] ]
  ]

tasks3 =
  [ Node (Command (TOUCH "/q/a") mempty)
      [ Node (Command (MKDIR "/q") (DIFFq "./a" "./b")) [] ]
  , Node (Command (TOUCH "/q/b") mempty)
      [ Node (Command (MKDIR "/q") (DIFFq "./c" "./d")) []
      , Node (Command (MKDIR "/q/p") mempty) [] ]
  ]


code2                        =  TaskL.script tasks2

main                         =  case code2 of
  Right (bytes, warnings)   ->  Data.ByteString.Lazy.hPut stdout bytes
  Left errors               ->  Data.ByteString.Lazy.hPut stderr "Error!"

