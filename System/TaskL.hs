{-# LANGUAGE StandaloneDeriving
           , TypeOperators
           , TypeFamilies
           , UndecidableInstances #-}
module System.TaskL where

import           Data.ByteString.Char8 (ByteString)
import           Data.Map (Map)
import           Data.Tree (Tree(..), Forest)

import qualified Language.Bash as Bash

import           System.TaskL.Strings (Label(), Name())


class Task phase where
  type Arg phase
  type Cmd phase
  type Cmd phase = Arg phase
  type Ref phase
  type Ref phase = Name

-- | Type of inputs that are allowed to contain template variables.
data Template = Template [Label] (Knot Template) deriving (Eq, Ord, Show)
instance Task Template where
  type Arg Template = [Either Label ByteString]
  type Ref Template = [Either Label Name]

-- | Type of inputs with remote references, requiring download.
newtype Partial = Partial (Knot Partial) deriving (Eq, Ord, Show)
instance Task Partial where
  type Arg Partial = ByteString
  type Cmd Partial = Either ByteString ByteString

-- | Type of inputs that are ready for serialization as Bash.
newtype Static = Static (Knot Static) deriving (Eq, Ord, Show)
instance Task Static where
  type Arg Static = ByteString

data Use t = Use { task :: Ref t,
                   args :: [Arg t],
                   with :: Forest (Use t) }
deriving instance (Eq (Arg t), Eq (Ref t)) => Eq (Use t)
deriving instance (Ord (Arg t), Ord (Ref t)) => Ord (Use t)
deriving instance (Show (Arg t), Show (Ref t)) => Show (Use t)

data Knot t = Knot { code :: Code t,
                     uses :: Forest (Use t),
                     asks :: Forest (Use t) }
deriving instance (Eq (Code t), Eq (Use t)) => Eq (Knot t)
deriving instance (Ord (Code t), Ord (Use t)) => Ord (Knot t)
deriving instance (Show (Code t), Show (Use t)) => Show (Knot t)

data Code t = Commands [(Cmd t, [Arg t])]
deriving instance (Eq (Arg t), Eq (Cmd t)) => Eq (Code t)
deriving instance (Ord (Arg t), Ord (Cmd t)) => Ord (Code t)
deriving instance (Show (Arg t), Show (Cmd t)) => Show (Code t)

data Module t = Module { from :: ByteString, defs :: Map Name t } 


-- | Compiler passes.
type a :~ b = a -> IO b 

template :: Module Template -> Map Name ByteString :~ Module Partial
template  = undefined

fetch :: Module Partial :~ Module Static
fetch  = undefined

bash :: Module Static :~ Bash.Annotated ()
bash  = undefined

deriving instance (Ord t) => Ord (Tree t)

