module Bf2Sat.Engine (run, ID(..)) where

import Bf2Sat.Parser (Tree(..))

data ID = ID {
  getPC :: Int,
  getMem :: [Int],
  getPT :: Int,
  getIC :: Int
} deriving (Show, Eq)

fixMem' ::[Int] -> Int -> Int -> [Int] -> [Int]
fixMem' [] pt delta acc = reverse acc
fixMem' (x:xs) 0 delta acc = reverse acc ++ [x + delta] ++ xs
fixMem' (x:xs) pt delta acc = fixMem' xs (pt-1) delta (x:acc)
fixMem :: [Int] -> Int -> Int -> [Int]
fixMem mem pt delta = fixMem' mem pt delta []

setMem' ::[Int] -> Int -> Int -> [Int] -> [Int]
setMem' [] pt delta acc = reverse acc
setMem' (x:xs) 0 delta acc = reverse acc ++ [x + delta] ++ xs
setMem' (x:xs) pt delta acc = setMem' xs (pt-1) delta (x:acc)
setMem :: [Int] -> Int -> Int -> [Int]
setMem mem pt delta = setMem' mem pt delta []

run' :: Int -> Int -> [Tree] -> Int -> [Int] -> Int -> [Int] -> Int-> [Int] -> [ID] -> ([ID], [Int])
run' maxStep step src pc inTape ic mem pt out ids =
  if step >= maxStep then
    (reverse ((ID pc mem pt ic):ids), reverse out)
  else if pc >= length src then
    run' maxStep (step+1) src pc inTape ic mem pt out (ID pc mem pt ic:ids)
  else case src !! pc of
    PtInc -> run' maxStep (step+1) src (pc+1) inTape ic mem (pt+1) out (ID pc mem pt ic:ids)
    PtDec -> run' maxStep (step+1) src (pc+1) inTape ic mem (pt+1) out (ID pc mem pt ic:ids)
    ValInc -> run' maxStep (step+1) src (pc+1) inTape ic (fixMem mem pt 1) pt out (ID pc mem pt ic:ids)
    ValDec -> run' maxStep (step+1) src (pc+1) inTape ic (fixMem mem pt (-1)) pt out (ID pc mem pt ic:ids)
    PutC -> run' maxStep (step+1) src (pc+1) inTape ic mem pt ((mem !! pt):out) (ID pc mem pt ic:ids)
    GetC -> run' maxStep (step+1) src (pc+1) inTape (ic+1) (setMem mem pt (inTape !! ic)) pt out (ID pc mem pt ic:ids)
    LoopBegin next -> run' maxStep (step+1) src (if 0 == mem !! pt then next else pc+1) inTape ic mem pt out (ID pc mem pt ic:ids)
    LoopEnd next -> run' maxStep (step+1) src (if 0 /= mem !! pt then next else pc+1) inTape ic mem pt out (ID pc mem pt ic:ids)


run :: [Tree] -> [Int] -> Int -> Int -> ([ID], [Int])
run src inTape memLen maxStep = run' maxStep 0 src 0 inTape 0 (take memLen [0,0..]) 0 [] []
