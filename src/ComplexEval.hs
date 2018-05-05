module CoplexEval where

import Data.Ratio

data Expression a = Val a | Mul (Expression a) (Expression a) | Add (Expression a)  (Expression a) | Sub (Expression a) (Expression a) | Div (Expression a) (Expression a) | Sqrt (Expression a) | Pow (Expression a ) (Expression a )deriving Eq

getLevel :: Expression a -> Int
getLevel (Mul _ _) = 1
getLevel (Add _ _) = 0
getLevel (Sub _ _) = 0
getLevel (Div _ _) = 1
getLevel (Sqrt _) = 2  
getLevel (Pow _ _) = 2
getLevel (Val _) = 2 


instance Show a => Show (Expression a) where
  show (Mul a b) = getShow 1 a ++ "*" ++ getShow 1 b
  show (Add a b) = getShow 0 a ++ "+" ++ getShow 0 b
  show (Sub a b) = getShow 0 a ++ "-" ++ getShow 0 b
  show (Div a b) = getShow 1 a ++ "/" ++ getShow 1 b
  show (Pow a b) = getShow 2 a ++ "^"++ getShow 2 b
  show (Sqrt r) = "("++ show r ++ ")^0.5"
  show (Val r) = show r

getShow :: Show a => Int -> Expression a -> String
getShow parentLevel expr = if parentLevel > getLevel expr then "(" ++ show expr ++ ")" else show expr 


simplifyKR :: Num a => Expression a -> Expression a
simplifyKR (Add (Val a) (Val b)) = Val (a + b)

{- 
tryAdd :: Num a => Expression a -> Expression a -> Expression a
tryAdd (Val a) (Val b) = Val (a + b)
tryAdd (Val a) (


takeOut :: Expression a -> Expression a -> Maybe (Expression a)
takeOut a b 
  | getLevel a /= getLevel b = Nothing
  | 
-}


