{-# LANGUAGE StandaloneDeriving
           , EmptyDataDecls
  #-}
module Language.TaskL.Syntax where

import Data.Monoid
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)

import Language.TaskL.Name
import Language.TaskL.StringL


data Module                  =  Module (Map Name TaskBody)

data TaskBody                =  TaskBody Test Task (Set Call)

data Call                    =  Call Name [StringL] (Set Call)

newtype Task                 =  Task [Code]

newtype Test                 =  Test [Code]

data Code                    =  Bash ByteString
                             |  Exec [StringL]
                             |  Hask (IO Bool)


--instance Monoid Module where
--  mempty                     =  Module mempty
--  mappend (Module x) (Module y) = Module (mappend x y)
--instance Monoid TaskBody where
--  mempty                     =  TaskBody Nothing Nothing mempty
--  mappend (TaskBody x y z) (TaskBody x' y' z') =
--    TaskBody (mappend x x') (mappend y y') (mappend z z')

