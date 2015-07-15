module Brainfuck2Sat.Engine (run, ID(..)) where

import Brainfuck2Sat.Parser (Tree(..))

data ID = ID {
  getPC :: Int,
  getMem :: [Int],
  getPT :: Int,
  getIC :: Int,
  getOC :: Int,
  getOut :: [Int]
} deriving (Show, Eq)

fixMem' ::[Int] -> Int -> Int -> [Int] -> [Int]
fixMem' [] _ _ acc = reverse acc
fixMem' (x:xs) 0 delta acc = reverse acc ++ [x + delta] ++ xs
fixMem' (x:xs) pt delta acc = fixMem' xs (pt-1) delta (x:acc)
fixMem :: [Int] -> Int -> Int -> [Int]
fixMem mem pt delta = fixMem' mem pt delta []

setMem' ::[Int] -> Int -> Int -> [Int] -> [Int]
setMem' [] _ _ acc = reverse acc
setMem' (_:xs) 0 val acc = reverse acc ++ [val] ++ xs
setMem' (x:xs) pt val acc = setMem' xs (pt-1) val (x:acc)

setMem :: [Int] -> Int -> Int -> [Int]
setMem mem pt delta = setMem' mem pt delta []

run' :: Int -> Int -> [Tree] -> Int -> [Int] -> Int -> [Int] -> Int-> [Int] -> [ID] -> [ID]
run' maxStep step src pc inTape ic mem pt out ids =
  if step >= maxStep then
    reverse ids
  else if pc >= length src then
    run' maxStep (step+1) src pc inTape ic mem pt out nextIDS
  else case src !! pc of
    PtInc -> run' maxStep (step+1) src (pc+1) inTape ic mem (pt+1) out nextIDS
    PtDec -> run' maxStep (step+1) src (pc+1) inTape ic mem (pt-1) out nextIDS
    ValInc -> run' maxStep (step+1) src (pc+1) inTape ic (fixMem mem pt 1) pt out nextIDS
    ValDec -> run' maxStep (step+1) src (pc+1) inTape ic (fixMem mem pt (-1)) pt out nextIDS
    PutC -> run' maxStep (step+1) src (pc+1) inTape ic mem pt ((mem !! pt):out) nextIDS
    GetC -> run' maxStep (step+1) src (pc+1) inTape (ic+1) (setMem mem pt (inTape !! ic)) pt out nextIDS
    LoopBegin next -> run' maxStep (step+1) src (if 0 == mem !! pt then next else pc+1) inTape ic mem pt out nextIDS
    LoopEnd next -> run' maxStep (step+1) src (if 0 /= mem !! pt then next else pc+1) inTape ic mem pt out nextIDS
  where nextIDS = ID pc mem pt ic (length out) (reverse out):ids

run :: [Tree] -> [Int] -> Int -> Int -> [ID]
run src inTape memLen maxStep = run' maxStep 0 src 0 inTape 0 (take memLen [0,0..]) 0 [] []
