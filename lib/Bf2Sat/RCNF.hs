module Bf2Sat.RCNF (fromDMACS) where

import Bf2Sat.SAT
import Data.List.Split (splitOn)
import Data.List (sortOn)

parseDMACS :: String -> [(Int, Bool)]
parseDMACS str = ansMap
  where
    ans = fmap (read :: String -> Int) $ splitOn " " $ head $ tail $ splitOn "\n" str
    0:available = reverse ans
    ansF p | p > 0 = (p,True)
           | otherwise = (-p,False)
    ansMap = fmap ansF (reverse available)

fromDMACS :: [(Int, Component)] -> String -> [(Component, Bool)]
fromDMACS preds str = filter predFilter predMap
  where
    r = parseDMACS str
    sortedR = sortOn fst r
    sortedP = sortOn fst preds
    zipped = zip sortedP sortedR
    matcher ((idx, p),(idx2,ans)) | idx == idx2 = (p,ans)
                                  | otherwise = error "???"
    predMap = fmap matcher zipped
    predFilter (Tmp _,_) = False
    predFilter _ = True
