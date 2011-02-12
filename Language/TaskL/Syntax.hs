{-# LANGUAGE StandaloneDeriving
           , EmptyDataDecls
  #-}
module Language.TaskL.Syntax where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)


data Module                  =  Module (Map Name TaskBody)

data TaskBody                =  TaskBody (Maybe Test) (Maybe Task) (Set Call)

data Call                    =  Call Name [StringL] TaskBody

newtype Name                 =  Name ByteString

newtype StringL              =  StringL [StringLToken]

data StringLToken            =  Literal ByteString
                             |  Substitution ByteString

newtype Task                 =  Task ByteString
deriving instance Monoid Task

newtype Test                 =  Test ByteString
deriving instance Monoid Test

instance Monoid Module where
  mempty                     =  Module mempty
  mappend (Module x) (Module y) = Module (mappend x y)
instance Monoid TaskBody where
  mempty                     =  TaskBody Nothing Nothing mempty
  mappend (TaskBody x y z) (TaskBody x' y' z') =
    TaskBody (mappend x x') (mappend y y') (mappend z z')
deriving instance Ord Name
deriving instance Eq Name
deriving instance Monoid Name
