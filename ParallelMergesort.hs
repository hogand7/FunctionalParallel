module Main
 where
 import System.Time
 import Control.Parallel
 import System.Random

 mergeSort :: (Ord a) => [a] -> [a]
 mergeSort [] = []
 mergeSort [a] = [a]
 mergeSort a =
  merge (mergeSort firstFew) (mergeSort lastFew)
   where firstFew = take ((length a) `div` 2) a
         lastFew = drop ((length a) `div` 2) a
          
 -- Expects a and b to already be sorted
 merge :: (Ord a) => [a] -> [a] -> [a]
 merge a [] = a
 merge [] b = b
 merge (a:as) (b:bs)
  | a < b     = a:(merge as (b:bs))
  | otherwise = b:(merge (a:as) bs)

 main :: IO ()
 main = do t0 <- getClockTime 
        let input = (take 20000 (randomRs (0,100) (mkStdGen 42)))::[Int]
        seq (forceList input) (return ())
        t1 <- getClockTime
        let r = sum (mergeSort input)
        seq r (return ())
        t2 <- getClockTime
        putStrLn (’’Sum of sort: ’’ ++ show r)
        putStrLn (’’Time to sort: ’’ ++ show (secDiff t1 t2))