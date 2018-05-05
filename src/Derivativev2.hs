module Derivativev2 where

import Data.Ratio

--import Control.Parallel

data Node a = X | Con a | Add (Node a) (Node a) | Sub (Node a) (Node a) | Mul (Node a) (Node a) | Div (Node a) (Node a) | Pow (Node a) (Node a) | Com (Node a) (Node a) | Exp | Log | Sin | Cos | Tan | Sec | Csc | Cot  deriving Eq

data PowerSeries a = PowerSeries (Int -> a)
data DPowerSeries a = DPowerSeries (Node a)
type PowerSeriesAprox a = (Int, PowerSeries a)


evaluatePSA :: Num a => PowerSeriesAprox a -> a -> a
evaluatePSA (n, PowerSeries seq) x = sum $ map (\ m -> seq m * (x -1) ^ m ) [0..n]

alt :: Int -> Int
alt = (^) (-1)

fac :: Int -> Int
fac 0 = 1
fac 1 = 1
fac n = n * fac (n - 1)

seq :: Int -> Rational
seq n = toInteger ((alt n) * fac (2 * n)) %  toInteger ( (1 - 2*n)*(fac n)^2 * 4^n)


instance (Show a, Eq a, Num a) => Show (Node a) where
  show X = "x"
  show (Con n) = show n
  show (Add n m) = show n ++ " + " ++ show m
  show (Sub n m) = show n ++ " - " ++ show m
  show (Mul n m) = if getNodeOrder n > 0 && getNodeOrder m > 0 then show n ++ "*" ++ show m else "(" ++ show n ++ ") * (" ++ show m ++ ")"
  show (Div n m) = if getNodeOrder n > 0 && getNodeOrder m > 0 then show n ++ "/"  ++ show m else "(" ++ show n ++ ") / (" ++ show m ++ ")"
  show (Pow n m) = "(" ++ show n ++ ") ^ (" ++ show m ++ ")"
  show (Com n m) = show n ++ "(" ++ show m ++ ")"
  show Log = "log"
  show Exp = "exp"
  show Sin = "sin"
  show Cos = "cos"
  show Tan = "tan"
  show Sec = "sec"
  show Csc = "csc"
  show Cot = "cot"
    where
      bob = 2

getNodeOrder :: Node a -> Int
getNodeOrder (Add _ _) = -1
getNodeOrder (Sub _ _) = -1
getNodeOrder (Mul _ _) = 0
getNodeOrder (Div _ _) = 0
getNodeOrder (Com _ _) = 1
getNodeOrder x  = 2


(~+) :: Node a -> Node a -> Node a
(~+) = Add

(~-) :: Node a -> Node a -> Node a
(~-) = Sub

(~*) :: Node a -> Node a -> Node a
(~*) = Mul

(~/) :: Node a -> Node a -> Node a
(~/) = Div

derivative :: (Floating a, Eq a) => Node a -> Node a
derivative (Add n m) = derivative n ~+ derivative m
derivative (Sub n m) = derivative n ~+ derivative m
derivative (Mul (Con a) m) = Con a ~* derivative m
derivative (Mul m (Con a)) = Con a ~* derivative m
derivative (Mul n m) = (derivative n ~* m) ~+ (derivative m ~* derivative n)
derivative (Div n m) = ((derivative n ~* m) ~- (n ~* derivative m)) ~/ ( Pow m (Con 2))
derivative (Pow n (Con a)) = (derivative n) ~* Con a ~* Pow n (Con (a -1))
derivative (Pow n m) = derivative $ Com Exp (m ~* Com Log n)
derivative (Com n m) = Com (derivative n) m ~* derivative m
derivative (Con n) = Con 0
derivative Exp = Exp
derivative Log = Div (Con 1) X
derivative Sin = Cos
derivative Cos = (Con (-1)) ~* Sin
derivative Tan = Pow Sec (Con 2)
derivative Sec = Con (-1) ~* Tan ~* Sec
derivative Csc = Con (-1) ~* Cot ~* Csc
derivative Cot = Con (-1) ~* (Pow Csc (Con 2))
derivative X = Con 1

simplify :: (Num a, Eq a) => Node a -> Node a
simplify (Add (Con 0) m) = simplify m
simplify (Add n (Con 0)) = simplify n
simplify (Add (Con n) (Con m)) = Con (n + m)
simplify (Add n m) = simplify n ~+ simplify m
simplify (Sub (Con 0) m) = simplify (Con (-1) ~* m)
simplify (Sub n (Con 0)) = simplify n
simplify (Sub (Con n) (Con m)) = Con (n - m)
simplify (Sub n m) = simplify n ~- simplify m
simplify (Mul (Con 0) _) = Con 0
simplify (Mul _ (Con 0)) = Con 0
simplify (Mul (Con 1) m) = m
simplify (Mul n (Con 1)) = n
simplify (Mul (Con n) (Con m)) = Con (m * n)
simplify (Mul n m) = simplify n ~* simplify m
simplify (Div n m ) = Div (simplify n) (simplify m)
simplify (Com X n) = simplify n
simplify (Com n X) = simplify n
simplify (Com n m) = Com (simplify n) (simplify m)
simplify X = X
simplify (Con n) = Con n
simplify (Pow n (Con 1)) = simplify n
simplify (Pow n m) = Pow (simplify n) (simplify m)
simplify Exp = Exp
simplify Log = Log
simplify Sin = Sin
simplify Cos = Cos
simplify Tan = Tan
simplify Sec = Sec
simplify Csc = Csc
simplify Cot = Cot

evaluate :: (Floating a, Eq a, Ord a) => Node a -> a -> Maybe a
evaluate (Con c) a = Just c
evaluate X x = Just x
evaluate (Add n m) x = fmap (+) (evaluate n x) <*> (evaluate m x)
evaluate (Sub n m) x = fmap (-) (evaluate n x) <*> (evaluate m x)
evaluate (Mul n m) x = fmap (*) (evaluate n x) <*> (evaluate m x)
evaluate (Div n m) x = fmap (/) (evaluate n x) <*> (evaluate m x)
evaluate (Pow n m) x = fmap (**) (evaluate n x) <*> (evaluate m x)
evaluate (Com n m) x =  (evaluate m x) >>= (evaluate n)
evaluate Exp x = Just (exp x)
evaluate Log x = if x > 0 then Just (log x) else Nothing
evaluate Sin x = Just (sin x)
evaluate Cos x = Just (cos x)
evaluate Tan x = Just (tan x)
evaluate Sec x = Just (1/ cos x)
evaluate Csc x = Just (1/ sin x)
evaluate Cot x = Just (1/ tan x)

{-
type Fracish a = Either a Rational

instance RealFloat a => Num (Fracish a) where
  (Left a) + (Left b) = Left ( a + b)
  (Right a) + (Right b) = Right (a + b)
  (Left a) + (Right b) = Left (a + fromRational b)
-}

{-
data SpecialValue = ValD Double | Val Rational | PI Rational | E Rational | Zero | One deriving (Show, Eq)

evalSV :: Node Double -> SpecialValue -> Maybe (Node SpecialValue)
evalSV (Add n m) v = Just bob
evalSV (Sub n m) v = Just bob
evalSV Exp v = SpecialValue exp





svExp :: SpecialValue -> SpecialValue
svExp ValD = exp
svEap ValR = ValD (exp $ fromRational r)
svExp PI r = ValD (exp $ pi * fromRational r)
svExp E r = ValD (exp $ exp r)
svExp Zero = One
svExp One = E 1

svLog :: SpecialValue -> Maybe SpecialValue
svLog ValD a = if a > 0 then Just ValD (log a) else Nothing
svLog ValR a = if a > 0 then Just ValD (log $ fromRational a) else Nothing
svLog PI a = if a > 0 then Just ValD (log $ pi * fromRational a) else Nothing
svLog E a = Valr a
svLog One = Zero

svSin


specialValueToRealFloat :: (RealFloat b) =>  SpecialValue b -> b
specialValueToRealFloat Val a = a
specialValueToRealFloat Pi r = pi * r
specialValueToRealFloat E r = r * exp 1
specialValueToRealFloat Zero = 0
specialValueToRealFloat LogVal a = log a
specialValueToRealFloat ExpVal a = exp a
-}

simp :: (Num a, Eq a) => Node a -> Node a
simp n = if ns == n then n else simp ns
  where ns = simplify n

derivativesAtZero :: (Floating a, Eq a, Ord a, Integral b) => Node a -> b -> [Maybe a]
derivativesAtZero f num_of_terms = map (\ x -> (flip evaluate) 0 $ nThDerivitive x f) [0..(num_of_terms -1)]

nThDerivitive :: (Floating a, Eq a, Integral b) => b -> Node a -> Node a
nThDerivitive 0 n = n
nThDerivitive i n = simp $ nThDerivitive (i -1) $ simp $ derivative n

nThDerivitives :: (Floating a, Eq a, Integral b) => b -> Node a -> [Node a]
nThDerivitives 0 n = [n]
nThDerivitives i n = n : (nThDerivitives (i -1) n')
  where
    n' = simp $ derivative n


nThDerivitive' :: (Floating a, Eq a, Integral b) => b -> Node a -> Node a
nThDerivitive' 0 n = n
nThDerivitive' i n = nThDerivitive (i -1) $ derivative n




