{-# LANGUAGE TypeFamilies
           , EmptyDataDecls
           , OverloadedStrings
           , StandaloneDeriving
           , ParallelListComp
  #-}


import System.IO
import Data.Tree
import Data.Monoid
import qualified Data.Binary.Builder
import qualified Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8

import System.TaskL.Bash
import System.TaskL.Bash.PrettyPrinter
import System.TaskL.IdemShell
import System.TaskL.Combination
import System.TaskL.IndexForest
import System.TaskL.Task
import System.TaskL.Op
import System.TaskL.Schedule
import System.TaskL


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


code2                        =  bytes
 where
  (ops, _, _)                =  schedule tasks2
  (arrays, install)          =  code ops
  text ppS t = Data.Binary.Builder.toLazyByteString (builder ppS t)
  bytes                      =  Data.ByteString.Lazy.Char8.unlines
                                  [ "# State arrays."
                                  , text (colPPState 0) arrays
                                  , ""
                                  , "# Installation routine."
                                  , "function apply {"
                                  , text (colPPState 2) install
                                  , "}"
                                  , "" ]

main                         =  Data.ByteString.Lazy.hPut stdout code2



