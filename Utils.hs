module Utils
  ( safeDiv
  , formatDouble
  , sortDescBy
  , splitOn
  , trim
  ) where

import Data.List (sortBy)
import Data.Char (isSpace)

-- safe division (returns 0 on division by zero)
safeDiv :: Double -> Double -> Double
safeDiv _ 0 = 0
safeDiv a b = a / b

-- Format double to 1 decimal place as string
formatDouble :: Double -> String
formatDouble d = show (fromIntegral (round (d * 10)) / 10 :: Double)

-- Sort list descending according to a projection to Double
sortDescBy :: (a -> Double) -> [a] -> [a]
sortDescBy f xs = sortBy (\a b -> compare (f b) (f a)) xs

-- Clear and simple splitOn for a single-char delimiter
splitOn :: Char -> String -> [String]
splitOn _ "" = [""]
splitOn delim s = go s []
  where
    go [] acc = [reverse acc]
    go (c:cs) acc
      | c == delim = reverse acc : go cs []
      | otherwise  = go cs (c:acc)

-- trim whitespace from both ends
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
