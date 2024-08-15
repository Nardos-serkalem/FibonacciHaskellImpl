import Data.Array

fibonacci :: Int -> Integer
fibonacci n
 | n<0 =  error "Negative Number are not allowed"
 | otherwise = fibArray ! n
  
  where
  fibArray :: Array Int Integer
  fibArray = array (0, n) [(i, fib i) | i <- [0..n]]
  fib 0=0
  fib 1=1
  fib i = fibArray ! (i-1) + fibArray ! (i-2)
  
main :: IO ()
main = mapM_ (print . fibonacci) [0..19]
