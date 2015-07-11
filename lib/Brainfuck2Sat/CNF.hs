{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Brainfuck2Sat.CNF (removeNot, toCNF, alias, toDMACS) where

import Brainfuck2Sat.SAT
import qualified Data.HashMap as M
import Data.HashMap ((!))
import qualified Data.List as L
import qualified Data.Witherable as W
import System.IO (withFile, IOMode( WriteMode ), Handle, hPutStr, hPutChar)
import Data.Hashable (Hashable, hash, hashWithSalt)
import Data.Tuple (swap)

removeNot :: Fml a -> Fml a
removeNot (Not (And a)) = Or $ fmap (removeNot . Not) a
removeNot (Not (Or a)) = And $ fmap (removeNot . Not) a
removeNot (Not (Not a)) = a
removeNot (Not (Pred a)) = Not (Pred a)
removeNot (And a) = And $ fmap removeNot a
removeNot (Or a) = Or $ fmap removeNot a
removeNot (Pred a) = Pred a

data CFml a = CNot a | CAff a deriving (Show)

instance (Hashable v) => Hashable (CFml v) where
  hashWithSalt s (CAff k) = s + (hash k)
  hashWithSalt s (CNot k) = s + (hash k) + 1

toCNF' :: [Int] -> Fml Component -> [[CFml Component]]
toCNF' addr (And xs) = zip [0..] xs >>= (\ (idx,it) -> toCNF' (idx:addr) it)
toCNF' _ (Or []) = [[]]
toCNF' addr (Or [x]) = toCNF' (0:addr) x
toCNF' addr (Or (x:y:xs))  = (zip [0..] cnfs) >>= appendFn
      where
        cnfs = fmap (\(i,it) -> toCNF' (i:0:addr) it) $ zip [1..] (x:y:xs)
        ntmps = length cnfs - 1
        tmps = fmap (\p -> Tmp (p:addr)) [1..ntmps]
        pos = fmap CAff tmps
        neg = fmap CNot tmps
        appendFn (idx, cls) = fmap (\cl -> app idx cl 0 pos neg) cls
        app _ cl x _ _ | x >= ntmps = cl
        app idx cl (cnt::Int) (pp:lpos) (np:lneg) =
                          app idx nclause (cnt+1) lpos lneg
                          where
                           nclause = if idx < cnt then cl else ((if idx == cnt then pp else np):cl)

toCNF' addr (Not (Pred p))  = [[CNot p]]
toCNF' _ (Pred p) = [[CAff p]]
toCNF' _ (Not _) = fail "????"

toCNF :: Fml Component -> [[CFml Component]]
toCNF xs = toCNF' [] xs

alias :: [[CFml Component]] -> ([[Int]], [(Int, Component)])
alias cnf = (fmap (fmap term2int) cnf, fmap swap $ M.toList dict)
  where
    getFml (CNot a) = a
    getFml (CAff a) = a
    preds = W.hashNub $ cnf >>= fmap getFml
    vs = zip preds [1..]
    dict = M.fromList vs
    term2int (CAff x) = dict ! x
    term2int (CNot x) = -(dict ! x)

toDMACS' :: [[Int]] -> [(Int, Component)] -> Handle -> IO ()
toDMACS' cnf dict handle = do
  hPutStr handle "p cnf "
  hPutStr handle $ show $ length dict
  hPutChar handle ' '
  hPutStr handle $ show $ length cnf
  hPutChar handle '\n'
  printCNF cnf
  where
    printCNF [] = return ()
    printCNF (x:xs) = printClause x >> printCNF xs
    printClause [] = hPutStr handle "0\n"
    printClause (x:xs) = do
      hPutStr handle $ show x
      hPutChar handle ' '
      printClause xs

toDMACS :: [[Int]] -> [(Int, Component)] -> FilePath -> IO()
toDMACS cnf dict fileName = withFile fileName WriteMode (toDMACS' cnf dict)
