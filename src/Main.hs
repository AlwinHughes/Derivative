module Main where

import Derivativev2

main :: IO ()
main = do
  {- print $ derivative' f 
  print $ nThDerivitive' 2 f
  print $ nThDerivitive' 3 f
  print $ nThDerivitive' 4 f
  print $ nThDerivitive' 5 f
  print $ nThDerivitive' 6 f
-}
  
  --let a = flip nThDerivitive 
  let f = Tan --(Tan ~* X) ~+ ((X ~^ (X ~+ Con 2) ~* Sin))
  let g = Com Log Csc 
  print $ nThDerivitives 4 g
  --print $ nThDerivitives 4 f
  --print $ map (flip evaluate 0) (nThDerivitives 10 f)
  print $ getTaylorPolynomial g 1.5 1.55 11 
  print $ Just ( log $ 1 / sin 1.55)
