module Brainfuck2Sat.Util (sortOn,calcBitLength, toBitList, showInTape) where

import qualified Data.List as L
import Data.List (sortBy)
import Data.Ord  ( comparing )

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))

calcBitLength :: Int -> Int
calcBitLength n = calcBitLength' 0 1
  where
    calcBitLength' c acc | n < acc = c
               | otherwise = calcBitLength' (c+1) (acc*2)

toBitList :: Int -> Int -> [Bool]
toBitList len n | len > 0 = ((n `mod` 2) == 1): toBitList (len-1) (n `div` 2)
                | otherwise = []

showInTape :: [Int] -> String
showInTape intape = "[" ++ L.intercalate ", " (fmap (\t -> if t < 0 then "(?)" else show t) intape) ++ "]"
