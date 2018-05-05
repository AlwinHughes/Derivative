module Main where

import Derivativev1

main :: IO ()
main = do
  let f = Tan'
  {- print $ derivative' f 
  print $ nThDerivitive' 2 f
  print $ nThDerivitive' 3 f
  print $ nThDerivitive' 4 f
  print $ nThDerivitive' 5 f
  print $ nThDerivitive' 6 f
-}
  print $ nThDerivitive' 7 f
  
