
module Language.TaskL.Override where

import Data.Set


data Override msg t          =  Override msg (OverrideCore t)


data OverrideCore t          =  Accept t
                             |  Select (Override t) (Set (Override t))
                             |  Keep (Set (Override t))
                             |  Merge t (Set (Override t))

