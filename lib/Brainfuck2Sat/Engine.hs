module Brainfuck2Sat.Engine (run, ID(..),fromSAT) where

import Brainfuck2Sat.Parser (Tree(..), Source(..))
import Brainfuck2Sat.SAT (Component(..), Time(..))
import Brainfuck2Sat.Util (calcBitLength)
import Data.Bits (setBit)

import qualified Data.HashMap.Strict as M

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

run' :: Int -> Int -> [Tree] -> Int -> Source -> Int -> [Int] -> Int-> [Int] -> [ID] -> [ID]
run' maxStep step ast pc src ic mem pt out ids =
  if step >= maxStep then
    reverse ids
  else if pc >= length ast then
    run' maxStep (step+1) ast pc src ic mem pt out nextIDS
  else case ast !! pc of
    PtInc -> run' maxStep (step+1) ast (pc+1) src ic mem (pt+1) out nextIDS
    PtDec -> run' maxStep (step+1) ast (pc+1) src ic mem (pt-1) out nextIDS
    ValInc -> run' maxStep (step+1) ast (pc+1) src ic (fixMem mem pt 1) pt out nextIDS
    ValDec -> run' maxStep (step+1) ast (pc+1) src ic (fixMem mem pt (-1)) pt out nextIDS
    PutC -> run' maxStep (step+1) ast (pc+1) src ic mem pt ((mem !! pt):out) nextIDS
    GetC -> run' maxStep (step+1) ast (pc+1) src (ic+1) (setMem mem pt (getInTape src !! ic)) pt out nextIDS
    LoopBegin next -> run' maxStep (step+1) ast (if 0 == mem !! pt then next else pc+1) src ic mem pt out nextIDS
    LoopEnd next -> run' maxStep (step+1) ast (if 0 /= mem !! pt then next else pc+1) src ic mem pt out nextIDS
  where nextIDS = ID pc mem pt ic (length out) (reverse out):ids

run :: Source -> [ID]
run src = run' maxStep 0 (getAST src) 0 src 0 (take memLen [0,0..]) 0 [] []
        where
          memLen = 2 ^ getAddrBits src
          maxStep = getSimStep src

fromSAT :: Source -> [(Component, Bool)] -> ([Int],[ID])
fromSAT src preds = (intape, ids)
  where
    hashmap = M.fromList preds
    ids :: [ID]
    simStep = getSimStep src
    ids = fmap recoverID [0..simStep - 1]
    recoverInt :: (Int -> Component) -> Int -> Int
    recoverInt (type_) bitLength = foldl recoverBit 0 [0..bitLength-1]
                                    where
                                      recoverBit :: Int -> Int -> Int
                                      recoverBit r ic = if bit then r `setBit` ic else r
                                                  where
                                                    Just bit = M.lookup (type_ ic) hashmap
    outLenBits = getOutAddrBits src
    outLen = 2 ^ outLenBits
    intape = fmap (\ic -> recoverInt (InTape ic) valueBits) [0..inLen - 1]
    outtape = fmap (\oc -> recoverInt (OutTape oc) valueBits) [0..outLen - 1]
    tapeLenBits = getAddrBits src
    tapeLen = 2 ^ tapeLenBits
    progLen = length $ getAST src
    progLenBits = calcBitLength progLen
    inLen = length $ getInTape src
    inLenBits = calcBitLength inLen
    valueBits = getValueBits src
    recoverID :: Int -> ID
    recoverID ti = ID pc mem pt ic oc (take oc outtape)
                  where
                    t = Time ti
                    pc = recoverInt (PC t) progLenBits
                    mem = fmap (\idx -> recoverInt (MidTape t idx) valueBits) [0..tapeLen-1]
                    pt = recoverInt (MC t) tapeLenBits
                    ic = recoverInt (IC t) inLenBits
                    oc = recoverInt (OC t) outLenBits
