module Derivative where

--import Control.Parallel

data Node a = Exp (Node a)| Sin (Node a) | Cos (Node a)| Log (Node a) | Tan (Node a) | Sec (Node a) | Csc (Node a) | Cot (Node a) | Xs a | X | Con a | Add (Node a) (Node a) | Sub (Node a) (Node a) | Mul (Node a) (Node a) | Div (Node a) (Node a) | Pow (Node a) (Node a) deriving Eq 

data Node2 a = X' | Con' a | Add' (Node2 a) (Node2 a) | Sub' (Node2 a) (Node2 a) | Mul' (Node2 a) (Node2 a) | Div' (Node2 a) (Node2 a) | Pow' (Node2 a) (Node2 a) | Com' (Node2 a) (Node2 a) | Exp' | Log' | Sin' | Cos' | Tan' | Sec' | Csc' | Cot'  deriving Eq


instance (Show a, Eq a, Num a) => Show (Node a) where
  show (Add n m) = (show n) ++ " + " ++ (show m)
  show (Sub n m) = (show n) ++ " - " ++ (show m)
  show (Mul n m) = "("++ (show n) ++ ") * (" ++ (show m) ++ ")"
  show (Div n m) = "(" ++ (show n) ++ ") / (" ++ (show m) ++ ")"
  show (Pow n m) = (show n) ++ " ^ " ++ (show m)
  show (Exp n) = "exp(" ++ (show n) ++ ")"
  show (Log n) = "log(" ++ (show n) ++ ")"
  show (Sin n) = "sin(" ++ (show n) ++ ")"
  show (Cos c) = "cos(" ++ (show c) ++ ")"
  show X = "x"
  show (Xs 1) = "x" 
  show (Xs c) = (show c) ++ "x" 
  show (Con c) = show c
  show (Tan n) = "tan(" ++ show n ++ ")"
  show (Sec n) = "sec(" ++ show n ++ ")"
  show (Csc n) = "csc(" ++ show n ++ ")"
  show (Cot n) = "cot(" ++ show n ++ ")"

instance (Show a, Eq a, Num a) => Show (Node2 a) where
  show X' = "x"
  show (Con' n) = show n
  show (Add' n m) = show n ++ " + " ++ show m
  show (Sub' n m) = show n ++ " - " ++ show m
  show (Mul' n m) = "(" ++ show n ++ ") * (" ++ show m ++ ")"
  show (Div' n m) = "(" ++ show n ++ ") / (" ++ show m ++ ")"
  show (Pow' n m) = "(" ++ show n ++ ") ^ (" ++ show m ++ ")"
  show (Com' n m) = show n ++ "(" ++ show m ++ ")"
  show Log' = "log"
  show Exp' = "exp"
  show Sin' = "sin"
  show Cos' = "cos"
  show Tan' = "tan"
  show Sec' = "sec"
  show Csc' = "csc"
  show Cot' = "cot"


{-
instance Functor Node where
  fmap f (Add n m) = (fmap f n) +~ (fmap f m)
  fmap f (Sub n m) = (fmap f n) -~ (fmap f m)
  fmap f (Mul n m) = (fmap f n) *~ (fmap f m)
  fmap f (Div n m) = (fmap f n) /~ (fmap f m)
  fmap f (Pow n m) = Pow (fmap f n) (fmap f m)
  fmap f (Exp n) = Exp (fmap f n)
  fmap f (Log n) = Log (fmap f n)
  fmap f (Sin n) = Sin (fmap f n)
  fmap f (Cos n) = Cos (fmap f n)
  fmap _ X = X
  fmap f (Xs n) = Xs (f n)
  fmap f (Con n) = Con (f n)
  fmap f (Tan n) = Tan (fmap f n)
  fmap f (Sec n) = Sec (fmap f n)
  fmap f (Csc n) = Csc (fmap f n)
  fmap f (Cot n) = Cot (fmap f n)


instance Functor Node where
  fmap f (Add m n) = Add (f m) (f n)
  fmap f (Sub m n) = Sub (f m) (f n)
  fmap f (Mul m n) = Mul (f m) (f n)
  fmap f (Div m n) = Div (f m) (f n)
  fmap f (Pow m n) = Pow (f m) (f n)
  fmap f (Exp m) = Exp (f m)
  fmap f (Log m) = Log (f m)
  fmap f (Sin m) = Sin (f m)
  fmap f (Cos m) = Cos (f m)
  fmap f (Tan m) = Tan (f m)
  fmap f (Sec m) = Sec (f m)
  fmap f (Csc m) = Csc (f m)
  fmap f (Cot m) = Cot (f m)
  fmap f (Xs c) = (Xs c)
-}
 

(+~) :: Node a -> Node a -> Node a
n +~ m = Add n m

(-~) :: Node a -> Node a -> Node a
n -~ m = Sub n m

(*~) :: Node a -> Node a -> Node a
n *~ m = Mul n m

(/~) :: Node a -> Node a -> Node a
n /~ m = Div n m


(~+) :: Node2 a -> Node2 a -> Node2 a
(~+) = Add'

(~-) :: Node2 a -> Node2 a -> Node2 a
(~-) = Sub'

(~*) :: Node2 a -> Node2 a -> Node2 a
(~*) = Mul'

(~/) :: Node2 a -> Node2 a -> Node2 a
(~/) = Div' 

safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x 
  | x <= 0 = Nothing
  | otherwise = Just (log x)

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeFun x y = Just (x / y)

evaluate :: (Floating a, Eq a) => Node a -> a -> Maybe a
evaluate (Con c) _ = Just c
evaluate X x = Just x
evaluate (Xs n) x = Just (x * n)
evaluate (Add n m) x = fmap (+) (evaluate n x) <*> (evaluate m x)
evaluate (Sub n m) x = fmap (-) (evaluate n x) <*> (evaluate m x)
evaluate (Mul n m) x = fmap (*) (evaluate n x) <*> (evaluate m x)
evaluate (Div n m) x = fmap (/) (evaluate n x) <*> (evaluate m x)
evaluate (Pow n m) x = fmap (**) (evaluate n x) <*> (evaluate m x)
evaluate (Exp n)  x = fmap exp (evaluate n x)
evaluate (Log n) x = fmap log (evaluate n x)
evaluate (Sin n) x = fmap sin (evaluate n x)
evaluate (Cos n) x = fmap cos (evaluate n x)

derivative :: (Floating a, Eq a) => Node a -> Node a
derivative X = Con 1
derivative (Con c) = Con 0
derivative (Xs c) = Con c
derivative (Pow n (Con c)) = Con c *~ (Pow n (Con (c-1))) *~ derivative n
derivative (Add n m) = derivative n +~ derivative m
derivative (Sub n m) = derivative n +~ derivative m
derivative (Mul (Con a) m) = Con a *~ derivative m
derivative (Mul m (Con a)) = Con a *~ derivative m
derivative (Mul n m) = (derivative n *~ m) +~ (derivative m *~ derivative n)
derivative (Div n m) = ((derivative n *~ m) -~ (n *~ derivative m)) /~ ( Pow m (Con 2))
derivative (Exp n) = derivative n *~ Exp n
derivative (Log n) = derivative n /~ n
derivative (Sin n) = Cos n *~ derivative n
derivative (Cos n) = Con (-1) *~ Sin n *~ derivative n
derivative (Tan n) = derivative n *~ Pow (Sec n) (Con 2)
derivative (Sec n) = Tan n *~ Sec n *~ derivative n 
derivative (Csc n) = Con (-1) *~ Cot n *~ Csc n *~ derivative n 
derivative (Cot n) = Con (-1) *~ (Pow (Csc n) (Con 2)) *~ derivative n 

derivative' :: (Floating a, Eq a) => Node2 a -> Node2 a
derivative' (Add' n m) = derivative' n ~+ derivative' m
derivative' (Sub' n m) = derivative' n ~+ derivative' m
derivative' (Mul' (Con' a) m) = Con' a ~* derivative' m
derivative' (Mul' m (Con' a)) = Con' a ~* derivative' m
derivative' (Mul' n m) = (derivative' n ~* m) ~+ (derivative' m ~* derivative' n)
derivative' (Div' n m) = ((derivative' n ~* m) ~- (n ~* derivative' m)) ~/ ( Pow' m (Con' 2))
derivative' (Pow' n (Con' a)) = (derivative' n) ~* Con' a ~* Pow' n (Con' (a -1)) 
derivative' (Pow' n m) = derivative' $ Com' Exp' (m ~* Com' Log' n)
derivative' (Com' n m) = Com' (derivative' n) m ~* derivative' m
derivative' (Con' n) = Con' 0
derivative' Exp' = Exp'
derivative' Log' = Div' (Con' 1) X'
derivative' Sin' = Cos' 
derivative' Cos' = (Con' (-1)) ~* Sin'
derivative' Tan' = Pow' Sec' (Con' 2)
derivative' Sec' = Tan' ~* Sec' 
derivative' Csc' = Con' (-1) ~* Cot' ~* Csc'
derivative' Cot' = Con' (-1) ~* (Pow' Csc' (Con' 2))
derivative' X' = Con' 1

simplify' :: (Num a, Eq a) => Node2 a -> Node2 a 
simplify' (Add' (Con' 0) m) = simplify' m
simplify' (Add' n (Con' 0)) = simplify' n
simplify' (Add' (Con' n) (Con' m)) = Con' (n + m)
simplify' (Add' n m) = simplify' n ~+ simplify' m
simplify' (Sub' (Con' 0) m) = simplify' (Con' (-1) ~* m)
simplify' (Sub' n (Con' 0)) = simplify' n
simplify' (Sub' (Con' n) (Con' m)) = Con' (n - m)
simplify' (Sub' n m) = simplify' n ~- simplify' m
simplify' (Mul' (Con' 0) _) = Con' 0 
simplify' (Mul' _ (Con' 0)) = Con' 0 
simplify' (Mul' (Con' 1) m) = m
simplify' (Mul' n (Con' 1)) = n
simplify' (Mul' (Con' n) (Con' m)) = Con' (m * n)
simplify' (Mul' n m) = simplify' n ~* simplify' m
simplify' (Div' n m ) = Div' (simplify' n) (simplify' m)
simplify' (Com' X' n) = simplify' n
simplify' (Com' n X') = simplify' n
simplify' (Com' n m) = Com' (simplify' n) (simplify' m)
simplify' X' = X'
simplify' (Con' n) = Con' n
simplify' (Pow' n (Con' 1)) = simplify' n 
simplify' (Pow' n m) = Pow' (simplify' n) (simplify' m)
simplify' Exp' = Exp'
simplify' Log' = Log'
simplify' Sin' = Sin'
simplify' Cos' = Cos'
simplify' Tan' = Tan'
simplify' Sec' = Sec'
simplify' Csc' = Csc'
simplify' Cot' = Cot'

evaluate' :: (Floating a, Eq a, Ord a) => Node2 a -> a -> Maybe a
evaluate' (Con' c) a = Just c
evaluate' X' x = Just x
evaluate' (Add' n m) x = fmap (+) (evaluate' n x) <*> (evaluate' m x)
evaluate' (Sub' n m) x = fmap (-) (evaluate' n x) <*> (evaluate' m x)
evaluate' (Mul' n m) x = fmap (*) (evaluate' n x) <*> (evaluate' m x)
evaluate' (Div' n m) x = fmap (/) (evaluate' n x) <*> (evaluate' m x)
evaluate' (Pow' n m) x = fmap (**) (evaluate' n x) <*> (evaluate' m x)
evaluate' (Com' n m) x =  (evaluate' m x) >>= (evaluate' n) 
evaluate' exp' x = Just (exp x)
evaluate' Log' x = if x > 0 then Just (log x) else Nothing
evaluate' Sin' x = Just (sin x)
evaluate' Cos' x = Just (cos x)
evaluate' Tan' x = Just (tan x)
evaluate' Sec' x = Just (1/ cos x)
evaluate' Csc' x = Just (1/ sin x)
evaluate' Cot' x = Just (1/ tan x)


simp' :: (Num a, Eq a) => Node2 a -> Node2 a 
simp' n = if ns == n then n else simp' ns
  where ns = simplify' n

derivativesAtZero :: (Floating a, Eq a, Ord a, Integral b) => Node2 a -> b -> [Maybe a]
derivativesAtZero f num_of_terms = map (\ x -> (flip evaluate') 0 $ nThDerivitive' x f) [0..(num_of_terms -1)]

nThDerivitive' :: (Floating a, Eq a, Integral b) => b -> Node2 a -> Node2 a
nThDerivitive' 0 n = n
nThDerivitive' i n = nThDerivitive' (i -1) $ simp' $ derivative' n

nThDerivitive :: (Floating a, Eq a, Integral b) => b -> Node a -> Node a
nThDerivitive 0 n = n
nThDerivitive i n = nThDerivitive (i -1) $ derivative n

simp :: (Num a, Eq a) => Node a -> Node a 
simp n = if ns == n then n else simp ns
  where ns = simplify n


simplify :: (Num a, Eq a) => Node a -> Node a 
simplify (Add (Con 0) n) = simplify n
simplify (Add (Con c) (Con d)) = Con (c + d)
simplify (Add n (Con 0)) = simplify n
simplify (Add m n) = Add (simplify m) (simplify n)
--simplify (Mul m (Con -1)) = Add (
simplify (Mul n (Con 0)) = Con 0
simplify (Mul (Con 0) n) = Con 0
simplify (Mul n (Con 1)) = simplify n
simplify (Mul (Con 1) n) = simplify n
simplify (Mul (Pow a b) (Pow c d)) 
  | a == c = simplify (Pow a (Add b d))
  | b == d = simplify (Pow (Mul a c) b)
  | otherwise = Mul (simplify (Pow a b)) (simplify (Pow c d))
simplify (Mul m n) 
  | n == m =  simplify $ Pow (simplify n) (Con 2)
  | otherwise = Mul (simplify m) (simplify n)
simplify (Mul n (Pow m c)) 
  | n == m = simplify $ Pow (simplify n) (simplify (Add c (Con 1)))
  | otherwise =  simplify $ Mul (simplify n) (simplify (Pow m c))
simplify (Mul (Pow n c) m) 
  | n == m =  simplify $ Pow (simplify n) (simplify (Add c (Con 1)))
  | otherwise = simplify $ Mul (simplify (Pow m c)) (simplify n)
simplify (Pow n (Con 1)) = simplify n
simplify (Pow m n) = Pow (simplify m) (simplify n)
simplify (Div m n) = Div (simplify m) (simplify n)
simplify (Exp n) = Exp (simplify n)
simplify (Log n) = Log (simplify n)
simplify (Sin n) = Sin (simplify n)
simplify (Cos n) = Cos (simplify n)
simplify X = X
simplify (Xs c) = Xs c
simplify (Con c) = Con c
simplify x = x

