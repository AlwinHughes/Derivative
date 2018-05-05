module Mandroots where 

import Data.Complex

data Qaudratic a = Qaudratic a a a deriving (Show, Eq)

solveQuadratic :: Qaudratic (Complex Double) -> (Complex Double, Complex Double)
solveQuadratic (Qaudratic a b c) = let d = sqrt (b*b - 4 * a * c) in ((-b + d)/(2 * a), (-b - d)/(2 * a)) 

evalQuadratic :: Num a => Qaudratic a -> a -> a
evalQuadratic (Qaudratic a b c) x =  x*x * a + x * b + c

rootsIteration :: Qaudratic (Complex Double) -> Qaudratic (Complex Double)
rootsIteration q@(Qaudratic a b c) = let (rt1, rt2) = solveQuadratic q in (Qaudratic rt1 rt2 c) 

type QCD = Qaudratic (Complex Double)

timeToEscape :: (QCD -> QCD) -> QCD -> Int -> Int
timeToEscape f q max = count q 0 
  where 
    count p n 
      | n ==  max = max
      | let a = evalQuadratic p (1 :+ 0) in realPart (a * conjugate a) > 25 = n
      | otherwise = count (f q) (n + 1)
