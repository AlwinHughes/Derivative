module Interval where

-- type based intervals
data TInterval a = TInterval (a -> Bool) 

iNot :: TInterval a -> TInterval a 
iNot (TInterval a)  = TInterval (not . a)

iUnion :: TInterval a -> TInterval a -> TInterval a
iUnion (TInterval i) (TInterval j) = TInterval (\ x -> i x || j x) 

iIntersect :: TInterval a -> TInterval a -> TInterval a
iIntersect (TInterval i) (TInterval j) = TInterval (\ x -> i x &&  j x) 

isIn :: a -> TInterval a 
isIn x (TInterval f) = f x


-- class based intervals
class Interval i where
  isin :: Ord a => a -> i a -> Bool

-- (a,b)
data OpenInterval a = OpenInterval a a 
instance Interval OpenInterval where
  isin x (OpenInterval a b) = x > a & x < b

-- [a,b]
data ClosedInterval a = ClosedInterval a a
instance Interval ClosedInterval where
  isin x (OpenInterval a b) = x >= a & x <= b

-- (a, Infty)
data FiniteToInfinityOpen a = FTIO a
instance Interval FiniteToInfinityOpen where
  isin x (FTIO a) = x > a

data FiniteToInfinityClose a = FTIC a
instance Interval FiniteToInfinityClose where
  isin x (FTIC a) = x >= a

data All a = All
instance Interval All where
  isin x (All) = True

data ExtendedReals a = Infinity | NegInfinity | Val a 

class Inter a where
  isinC :: a -> b -> Bool 

data PositiveReals = PositiveReals 
instance Inter PositiveReals where 
  isinC (PositiveReals) x = x > 0

-- (a,b)
data I a = I a a
instance Inter I where
  isinC (Inter a b) x = x > a && x < b


