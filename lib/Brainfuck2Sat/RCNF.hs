module Brainfuck2Sat.RCNF (fromDIMACS) where

import Brainfuck2Sat.SAT
import Brainfuck2Sat.Util
import Data.List.Split (splitOn)
import qualified Data.HashMap.Strict as M

parseDIMACS :: String -> [(Int, Bool)]
parseDIMACS str = ansMap
  where
    ans = fmap (read :: String -> Int) $ splitOn " " $ head $ tail $ splitOn "\n" str
    0:available = reverse ans
    ansF p  | p > 0 = (p,True)
        | otherwise = (-p,False)
    ansMap = fmap ansF (reverse available)

fromDIMACS :: [(Int, Component)] -> String -> M.HashMap Component Bool
fromDIMACS preds str = id $! makeHash zipped M.empty
  where
    r = parseDIMACS str
    sortedR = sortOn fst r
    sortedP = sortOn fst preds
    zipped = zip sortedP sortedR
    makeHash (((_, (Tmp _)),_):l) mp = makeHash l mp
    makeHash [] mp = mp
    makeHash (((idx, p),(idx2,ans)):l) mp =
        if (idx == idx2)
          then makeHash l (M.insert p ans mp)
          else error "???"
