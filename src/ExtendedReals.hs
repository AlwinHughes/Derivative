module ExtenedReals where

data ExtenedReals a = PosInfinity | NegInfinity | Val a | Undefined deriving (Show, Eq)

instance Num a => Num (ExtenedReals a) where
  PosInfinity + PosInfinity = PosInfinity
  PosInfinity + NegInfinity = Undefined
  PosInfinity + (Val a) = PosInfinity
  NegInfinity + PosInfinity = Undefined
  NegInfinity + NegInfinity = NegInfinity
  NegInfinity + (Val a) = NegInfinity
  (Val a ) + PosInfinity = PosInfinity
  (Val a ) + NegInfinity = NegInfinity
  (Val a ) + (Val b) = Val (a + b)
  Undefined + _ = Undefined
  _ + Undefined = Undefined

instance Ord a => Ord (ExtenedReals a) where
  compare Undefined _ = error " canot compare to Undefined"
  Undefined > _ = error " canot compare to Undefined"
  PosInfinity > _ = True

