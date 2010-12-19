
{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving
  #-}

module Language.TaskL.Task where

import Data.String ()
import Data.Monoid

import Data.ByteString

import qualified Language.TaskL.IdemShell as IdemShell
import Language.TaskL.Combination


{-| A task may be either a command or a named package. Commands provide their
    own labels. Every 'Task' may attach tests to restrict the circumstances
    under which a command is run.
 -}
data Task                    =  Command IdemShell.Command IdemShell.Test
                             |  Package ByteString IdemShell.Test
deriving instance Eq Task
deriving instance Show Task
instance Combine Task where
  combine a@(Command c0 t0) b@(Command c1 t1) = case combine c0 c1 of
    Combined c              ->  Combined (Command c (t0 `mappend` t1))
    Separate _ _            ->  Separate a b
    Contradictory _ _       ->  Contradictory a b
  combine a@(Package l0 t0) b@(Package l1 t1)
    | l0 == l1               =  Combined (Package l0 (t0 `mappend` t1))
    | otherwise              =  Separate a b
  combine a b                =  Separate a b


label                       ::  Task -> ByteString
label (Command c _)          =  IdemShell.label c
label (Package b _)          =  "task:" `append` b

