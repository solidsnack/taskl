{-# LANGUAGE OverloadedStrings
           , ParallelListComp
  #-}


import System.IO
import qualified Data.Set as Set
import Data.Tree
import Data.Monoid
import Data.Either
import qualified Data.Binary.Builder
import qualified Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8

import Data.Digest.Pure.SHA

import Language.TaskL.EncDec
import Language.TaskL.IdemShell
import Language.TaskL.IdemShell.PasswdDB
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

tasks3 = do
  root <- dec "root"
  admin <- dec "admin"
  admin' <- dec "admin"
  return
    [ Node (Command (GPASSWDm admin (Set.fromList ["solidsnack"]) Set.empty)
                    mempty)
           []
    , Node (Command (CP "./etc/sudoers" "/etc/sudoers") mempty) []
    , Node (Command (CHMOD "/etc/sudoers"
                           (Mode On On Off Off On Off Off Off Off Off Off Off))
                    mempty)
           [Node (Command (TOUCH "/etc/sudoers") mempty) []]
    , Node (Command (CHOWN "/etc/sudoers" (Both root admin')) mempty)
           [Node (Command (TOUCH "/etc/sudoers") mempty) []]
    ]

code2                        =  TaskL.bash tasks2

code3                        =  TaskL.bash (either (const []) id tasks3)

main                         =  case code3 of
  Right (bytes, warnings)   ->  Data.ByteString.Lazy.hPut stdout bytes
  Left errors               ->  Data.ByteString.Lazy.hPut stderr "Error!"

