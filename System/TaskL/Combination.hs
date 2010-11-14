{-# LANGUAGE StandaloneDeriving
  #-}

module System.TaskL.Combination where


data Combination t           =  Contradictory t t
                             |  Combined t
                             |  Separate t t
deriving instance (Eq t) => Eq (Combination t)
deriving instance (Ord t) => Ord (Combination t)
deriving instance (Show t) => Show (Combination t)
instance Functor Combination where
  fmap f (Contradictory a b) =  Contradictory (f a) (f b)
  fmap f (Combined a)        =  Combined (f a)
  fmap f (Separate a b)      =  Separate (f a) (f b)


class (Eq t) => Combine t where
  combine                   ::  t -> t -> Combination t


draw                        ::  Combination String -> String
draw (Contradictory a b)     =  "-----------Contradictory\n" ++ a ++ "\n" ++ b
draw (Combined a)            =  "----------------Combined\n" ++ a
draw (Separate a b)          =  "----------------Separate\n" ++ a ++ "\n" ++ b

