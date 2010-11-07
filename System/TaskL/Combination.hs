
module System.TaskL.Combination where

data Combination t           =  Contradictory t t
                             |  Combined t
                             |  Separate t t
 deriving (Eq, Ord, Show)

class (Eq t) => Combine t where
  combine                   ::  t -> t -> Combination t

contradictory               ::  Combination t -> Bool
contradictory (Contradictory _ _) = True
contradictory _              =  False

