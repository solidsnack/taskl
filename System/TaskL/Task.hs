{-# LANGUAGE StandaloneDeriving #-}
module System.TaskL.Task where

import           Data.ByteString.Char8 (ByteString)
import           Data.Map (Map)
import           Data.Tree (Tree(..), Forest)

import           System.TaskL.Strings (Label(), Name())


-- | A task, with its variable bindings and body.
data Task = Task [(Label, Maybe ByteString)] Knot
 deriving (Eq, Ord, Show)

-- | An instance of task use, similar to a command.
data Use = Use { task :: Name, args :: [[Either Label ByteString]] }
 deriving (Eq, Ord, Show)

-- | Type of connector between task code, a task's dependencies and a task's
--   requested post-actions.
data Knot = Knot { code :: Code,
                   deps :: Forest Use,
                   asks :: Forest Use }
 deriving (Eq, Ord, Show)

-- | A task body which, at present, is just a sequence of commands and their
--   arguments.
data Code = Commands [([Either Label ByteString], [[Either Label ByteString]])]
 deriving (Eq, Ord, Show)

data Module = Module { from :: ByteString, defs :: Map Name Task }
 deriving (Eq, Ord, Show)

deriving instance (Ord t) => Ord (Tree t)

