{-# LANGUAGE ExistentialQuantification #-}
module CompileCheckDerivative where

-- a is the input interval 
data Function a b = forall c. DefFunc c =>  Add (Function a b) (Function a b) | Sub (Function a b) (Function a b) | Mul (Function a b) (Function a b) | Div (Function a b) (Function a b) | Com (Function a b) (Function a b) | Pow (Function a b) (Function a b) | Box c a b

class Interval a where 
  isin :: a -> Bool



class DefFunc f where 
 -- derive ::  Interval a => f -> Function a
  evaluate ::  f -> c -> c


--data SB b = forall a. Show a => SB a b | SBB a




