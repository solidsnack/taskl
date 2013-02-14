{-# LANGUAGE StandaloneDeriving
           , TypeOperators
           , TypeFamilies
           , UndecidableInstances #-}
module System.TaskL.Phases where

import           Data.ByteString.Char8 (ByteString)
import           Data.Map (Map)
import           Data.Tree (Tree(..), Forest)

import qualified Language.Bash as Bash

import           System.TaskL.Strings (Label(), Name())


-- | Tasks take different forms depending on the compiler phase. Intermediate
--   formats can contain URLs to remote commands or template variables; a
--   final, static format can be compiled to Bash.
class Task phase where
  type Arg phase
  data Cmd phase
  type Ref phase
  type Ref phase = Name

-- | Type of inputs that are allowed to contain template variables.
data Templated = Templated [Label] (Knot Templated) deriving (Eq, Ord, Show)
instance Task Templated where
  type Arg Templated = [Either Label ByteString]
  type Ref Templated = [Either Label Name]
  data Cmd Templated = TemplatedCmd (Arg Templated) deriving (Eq, Ord, Show)

-- | Type of inputs with remote references, requiring download.
newtype WithURLs = WithURLs (Knot WithURLs) deriving (Eq, Ord, Show)
instance Task WithURLs where
  type Arg WithURLs = ByteString
  data Cmd WithURLs = Remote ByteString | Local (Cmd Static)
   deriving (Eq, Ord, Show)

-- | Type of inputs that are ready for serialization as Bash.
newtype Static = Static (Knot Static) deriving (Eq, Ord, Show)
instance Task Static where
  type Arg Static = ByteString
  data Cmd Static = Path ByteString | Exec ByteString deriving (Eq, Ord, Show)

-- | An instance of task use, which may have additional dependencies.
data Use t = Use { task :: Ref t,
                   args :: [Arg t],
                   adds :: Forest (Use t) }
deriving instance (Eq (Arg t), Eq (Ref t)) => Eq (Use t)
deriving instance (Ord (Arg t), Ord (Ref t)) => Ord (Use t)
deriving instance (Show (Arg t), Show (Ref t)) => Show (Use t)

-- | Type of connector between task code, a task's dependencies and a task's
--   requested post-actions.
data Knot t = Knot { code :: Code t,
                     uses :: Forest (Use t),
                     asks :: Forest (Use t) }
deriving instance (Eq (Code t), Eq (Use t)) => Eq (Knot t)
deriving instance (Ord (Code t), Ord (Use t)) => Ord (Knot t)
deriving instance (Show (Code t), Show (Use t)) => Show (Knot t)

-- | A task body which, at present, is just a sequence of commands and their
--   arguments.
data Code t = Commands [(Cmd t, [Arg t])]
deriving instance (Eq (Arg t), Eq (Cmd t)) => Eq (Code t)
deriving instance (Ord (Arg t), Ord (Cmd t)) => Ord (Code t)
deriving instance (Show (Arg t), Show (Cmd t)) => Show (Code t)

data Module t = Module { from :: ByteString, defs :: Map Name t }


-- | The type of compiler passes.
type a :~ b = a -> IO b

template :: Module Templated -> Map Name ByteString :~ Module WithURLs
template  = undefined

remotes :: Bool -- ^ If true, retrieve remote commands at compile time and
                --   inline them; otherwise, generate shell code to retrieve
                --   the commands with cURL and execute them.
        -> Module WithURLs :~ Module Static
remotes  = undefined

bash :: Module Static -> Bash.Annotated ()
bash  = undefined

deriving instance (Ord t) => Ord (Tree t)

