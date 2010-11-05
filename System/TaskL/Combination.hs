
module System.TaskL.Combination where

data Combination t           =  Contradictory t t
                             |  Combined t
                             |  Separate t t
 deriving (Eq, Ord, Show)

