module Bf2Sat.CNF (removeNot, toCNF, alias, toDMACS) where

import Bf2Sat.SAT
import Data.Map
import qualified Data.List as L
import qualified Data.Set as S

uniquify :: Ord a => [a] -> [a]
uniquify = S.toList . S.fromList

removeNot :: Fml a -> Fml a
removeNot (Not (And a)) = Or $ fmap (removeNot . Not) a
removeNot (Not (Or a)) = And $ fmap (removeNot . Not) a
removeNot (Not (Not a)) = a
removeNot (Not (Pred a)) = Pred a
removeNot (And a) = And $ fmap removeNot a
removeNot (Or a) = Or $ fmap removeNot a
removeNot (Pred a) = Pred a

data CFml a = CNot a | CAff a deriving (Show)

mergeTwo :: [[CFml Component]] -> [[CFml Component]] -> Int -> [Int] -> [[CFml Component]]
mergeTwo xsat ysat cnt addr =
              fmap (\ors -> t : ors) xsat ++ fmap (\ors -> nt:ors) ysat
              where
                t = CAff (Tmp (cnt:addr))
                nt = CNot (Tmp (cnt:addr))

mergeOr :: Fml Component -> Int -> [Int] -> [[CFml Component]]
mergeOr (Or []) _ _ = [[]]
mergeOr (Or [_]) _ _ = fail "???"
mergeOr (Or [x,y]) cnt addr = mergeTwo (toSat' x (cnt:addr)) (toSat' y (cnt:addr)) cnt addr
mergeOr (Or (x:y:xs)) cnt addr = mergeTwo (toSat' x (cnt:addr)) (mergeOr (Or (y:xs)) (cnt+1) addr) cnt addr
mergeOr _ _ _ = fail "????"

toSat' :: Fml Component -> [Int] -> [[CFml Component]]
toSat' (And xs) addr = zip [0..] xs >>= (\ (idx,it) -> toSat' it (idx:addr))
toSat' (Or []) _ = [[]]
toSat' (Or [x]) addr = toSat' x (0:addr)
toSat' (Or (x:y:xs)) addr = mergeOr (Or (x:y:xs)) 0 addr
toSat' (Not (Pred p)) _ = [[CNot p]]
toSat' (Pred p) _ = [[CAff p]]
toSat' (Not _) _ = fail "????"

c2s :: CFml Component -> CFml String
c2s (CAff x) = CAff $ show x
c2s (CNot x) = CNot $ show x

toCNF :: Fml Component -> [[CFml Component]]
toCNF xs = toSat' xs []

toNorm :: [[CFml Component]] -> [[CFml String]]
toNorm xs = fmap (fmap c2s) xs

alias :: [[CFml Component]] -> ([[Int]], [(String, Int)])
alias cnf = (fmap (fmap term2int) norm, vs)
  where
    norm = toNorm cnf
    getFml (CNot a) = a
    getFml (CAff a) = a
    flatten = norm >>= fmap getFml
    uniq = uniquify flatten
    vs = zip uniq [1..]
    dict = fromList vs
    term2int (CAff x) = dict ! x
    term2int (CNot x) = -(dict ! x)

toDMACS :: [[Int]] -> [(String, Int)] -> String
toDMACS cnf dict = "p cnf "++ nvariable++" "++nand++"\n"++left++" 0"
  where
    nvariable = show $ length dict
    nand = show $ length cnf
    terms = fmap (L.intercalate " ") (fmap (fmap show) cnf)
    left = L.intercalate " 0\n" terms
