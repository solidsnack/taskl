{-# LANGUAGE StandaloneDeriving
           , EmptyDataDecls
  #-}
module Language.TaskL.Syntax where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)

import Language.TaskL.StringL


data Module                  =  Module (Map Name TaskBody)

data TaskBody                =  TaskBody (Maybe Test) (Maybe Task) (Set Call)

data Call                    =  Call Name [StringL] (Set Call)

newtype Name                 =  Name ByteString

newtype Task                 =  Task Code

newtype Test                 =  Test Code

data Code                    =  Bash ByteString | Exec [StringL]


--instance Monoid Module where
--  mempty                     =  Module mempty
--  mappend (Module x) (Module y) = Module (mappend x y)
--instance Monoid TaskBody where
--  mempty                     =  TaskBody Nothing Nothing mempty
--  mappend (TaskBody x y z) (TaskBody x' y' z') =
--    TaskBody (mappend x x') (mappend y y') (mappend z z')
deriving instance Ord Name
deriving instance Eq Name
deriving instance Monoid Name
