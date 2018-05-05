{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module Derivativev3 where

data Function =
  Add Function Function |
  Sub Function Function |
  Mul Function Function |
  Div Function Function |
  Com Function Function |
  Pow Function Function |
  forall c. DefFunc c =>Box c

instance Show Function where
  show (Add f g) = "(" ++ show f ++ ")+(" ++ show g ++")"
  show (Sub f g) = "(" ++ show f ++ ")-(" ++ show g++")"
  show (Mul f g) = "(" ++ show f ++ ")*(" ++ show g++")"
  show (Div f g) = "(" ++ show f ++ ")/(" ++ show g++")"
  show (Pow f g) = "(" ++ show f ++ ")^(" ++ show g++")"
  show (Com f g) = "(" ++ show f ++ "(" ++ show g ++ ")"
  show (Box f) = showf f

data One = One deriving (Eq, Show)
data Const a = Const a deriving (Eq, Show)
data Zero = Zero deriving (Eq, Show)
data X = X deriving (Eq, Show)
data Sin = Sin deriving (Eq, Show)
data Cos = Cos deriving (Eq, Show)
data Exp = Exp deriving (Eq, Show)
data Log = Log deriving (Eq, Show)
data Tan = Tan deriving (Eq, Show)
data Sec = Sec deriving (Eq, Show)
data Csc = Csc deriving (Eq, Show)
data Cot = Cot deriving (Eq, Show)

--negOne :: (ZeroOne a, Num a) => Function a
--negOne = Sub (Box (Zero 0)) (Box (One 0))

class DefFunc f where
  derive :: f -> Function
  evaluate :: f -> Double -> Maybe Double
  showf :: f -> String

evaluateFunction :: Function -> Double -> Maybe Double
evaluateFunction (Add f g) x = ((+) <$> (evaluateFunction f x )) <*> ( evaluateFunction f x)
evaluateFunction (Sub f g) x = ((-) <$> (evaluateFunction f x )) <*> ( evaluateFunction f x)
evaluateFunction (Mul f g) x = ((*) <$> (evaluateFunction f x )) <*> ( evaluateFunction f x)
evaluateFunction (Div f g) x = ((/) <$> (evaluateFunction f x )) <*> ( evaluateFunction f x)
evaluateFunction (Pow f g) x = ((**) <$> (evaluateFunction f x )) <*> ( evaluateFunction f x)
evaluateFunction (Com f g) x = ( evaluateFunction f x) >>= (evaluateFunction f)
evaluateFunction (Box f) x = evaluate f x

derivativeFunction :: Function -> Function
derivativeFunction (Add f g) = derivativeFunction f +~ derivativeFunction g
derivativeFunction (Sub f g) = derivativeFunction f -~ derivativeFunction g
derivativeFunction (Mul f g) = (derivativeFunction f *~ g ) +~ (derivativeFunction g *~ f)
derivativeFunction (Div f g) = (derivativeFunction f *~ g) -~ (derivativeFunction f *~ g)
derivativeFunction (Pow f (Box (Const a))) = derivativeFunction f *~ Pow f (Box (Const (a - 1)))
derivativeFunction (Com f g) = ((derivativeFunction f) .~ g) *~ derivativeFunction g
derivativeFunction (Box f) = derive f


class Floating a => ZeroOne a where
  zero :: a
  one :: a

instance ZeroOne Double where
  zero = 0 :: Double
  one = 1 :: Double

instance ZeroOne Float where
  zero = 0 :: Float
  one = 1 :: Float

instance  DefFunc One where
  derive One = Box Zero
  evaluate _ _ = Just one
  showf _ = "1"

instance DefFunc Zero where
  derive Zero = Box Zero
  evaluate Zero x = Just zero
  showf _ = "0"

instance DefFunc (Const Double) where
  derive (Const _) = Box Zero
  evaluate (Const a) _ = Just a
  showf (Const a) = show a

instance DefFunc X where
  derive X = Box One
  evaluate X x = Just x
  showf _ = "x"

instance DefFunc Sin where
  derive Sin = Box Cos
  evaluate Sin = Just . sin
  showf _ = "sin"

instance DefFunc Cos where
  derive Cos = ((Box Zero) -~ (Box One)) *~ (Box Sin )
  evaluate Cos = Just . cos
  showf _ = "cos"

instance DefFunc Exp where
  derive Exp = Box Exp
  evaluate Exp = Just . exp
  showf _ = "exp"

instance DefFunc Log where
  derive Log = Div (Box One) (Box X)
  evaluate Log = Just . log
  showf _ = "log"

instance DefFunc Tan where
  derive Tan = Box Sec ^~ Box (Const (2.0 :: Double))
  evaluate Tan = Just . tan
  showf Tan = "tan"

instance DefFunc Sec where
  derive Sec = Box Tan *~ Box Sec
  evaluate Sec = Just . (1.0/) . cos
  showf Sec = "sec"

instance DefFunc Csc where
  derive Csc = Box (Const (-1.0 :: Double)) *~ Box Cot *~ Box Csc
  evaluate Csc = Just . (1.0/) . sin
  showf Csc = "csc"

instance DefFunc Cot where
  derive Cot = Box (Const  (-1.0 :: Double)) *~ ( Box Csc ^~ Box (Const  (2.0 :: Double)))
  evaluate Cot = Just . (1.0/) . tan
  showf Cot = "cot"




(+~) :: Function -> Function -> Function
(+~) = Add

(-~) :: Function -> Function -> Function
(-~) = Sub

(*~) :: Function -> Function -> Function
(*~) = Mul

(/~) :: Function -> Function -> Function
(/~) = Div

(^~) :: Function -> Function -> Function
(^~) = Pow

(.~) :: Function -> Function -> Function
(.~) = Com


{-
data Function a =
  forall c. DefFunc c =>Add (c a) (c a)  |
  forall c. DefFunc c =>Sub (c a) (c a) |
  forall c. DefFunc c =>Mul (c a) (c a) |
  forall c. DefFunc c =>Div (c a) (c a) |
  forall c. DefFunc c =>Com (c a) (c a) |
  forall c. DefFunc c =>Pow (c a) (c a) |
  forall c. DefFunc c =>Box (c a)
-}
