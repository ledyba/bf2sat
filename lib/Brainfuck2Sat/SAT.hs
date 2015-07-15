module Brainfuck2Sat.SAT (Time(..), Component(..), Fml(..), States, gen, valueMax, outLen, tapeLen, timeLen) where
import Brainfuck2Sat.Parser as P

import Control.Applicative ((<$>))
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Control.Arrow as CA

--------------------------------------------------------------------------------

timeLen :: Int
timeLen = 140

valueBits :: Int
valueBits = 7

valueMax :: Int
valueMax = (2 ^ valueBits)-1

tapeLenBits :: Int
tapeLenBits = 4

tapeLen :: Int
tapeLen = 2 ^ tapeLenBits

outLenBits :: Int
outLenBits = 4

outLen :: Int
outLen = 2 ^ outLenBits

--------------------------------------------------------------------------------

newtype Time = Time {getTime :: Int}  deriving (Eq,Ord)
data Component =
    PC Time Int |
    IC Time Int |
    InTape Int Int |
    MC Time Int |
    MidTape Time Int Int |
    OC Time Int |
    OutTape Int Int |
    Tmp [Int]
  deriving (Eq,Show,Read,Ord)
data Fml a = And [Fml a] | Or [Fml a] | Not (Fml a) | Pred a deriving (Show, Read, Eq)
type States = Fml Component
type PC = Int

instance Hashable Time where
  hashWithSalt s k = s + hash (getTime k)

instance Hashable Component where
  hashWithSalt s (PC t i) = s +　hash t + hash i + 9854037
  hashWithSalt s (IC t i) = s +　hash t + hash i + 4839704
  hashWithSalt s (MC t i) = s + hash t + hash i + 1875123
  hashWithSalt s (OC t i) = s + hash t + hash i + 8751067
  hashWithSalt s (InTape t i) = s + hash t + hash i + 21053
  hashWithSalt s (OutTape t i) = s + hash t + hash i + 1234532
  hashWithSalt s (MidTape t i j) = s + hash t + hash i + hash j + 10234552
  hashWithSalt s (Tmp k) = s + hash k + 3054521435
instance Show Time where
  showsPrec d t = showsPrec d (getTime t)

instance Read Time where
  readsPrec d s = fmap (CA.first Time) (readsPrec d s)

calcBitLength :: Int -> Int
calcBitLength n = calcBitLength' 0 1
  where
    calcBitLength' c acc | n > acc = calcBitLength' (c+1) (acc*2)
                         | otherwise = c

toBitList :: Int -> Int -> [Bool]
toBitList n len | len > 0 = ((n `mod` 2) == 1):(toBitList (n `div` 2) (len-1))
                | otherwise = []

prod :: [a] -> [a] -> [(a,a)]
prod a b = concat $ fmap ( \it1 -> fmap (\it2 -> (it1,it2) ) b ) a

--------------------------------------------------------------------------------

gen :: [P.Tree] -> [Int] -> States
gen src inTape =
            And [
              genInitState progLen inTape
              ,genLastState progLen
              ,genMiddleState src inTapeLength
            ] where
              progLen = length src
              inTapeLength = length inTape


t0 :: Time
t0 = Time 0

lastTime :: Time
lastTime = Time (timeLen-1)

--------------------------------------------------------------------------------

incTime :: Time -> Time
incTime t = Time $ 1 + getTime t

incPc :: PC -> PC
incPc t = t + 1

--------------------------------------------------------------------------------

makeConst :: (Int -> Component) -> Int -> Int -> Fml Component
makeConst type_ bitLength value = And $ fmap (\(bi,b) -> if b then Pred $ type_ bi else Not (Pred (type_ bi))) (zip [0..] (toBitList value bitLength))

isConst :: (Int -> Component) -> Int -> Int -> Fml Component
isConst = makeConst

makeEqP :: Component -> Component -> Fml Component
makeEqP from_ to_ = makeEqFml (Pred from_) (Pred to_)

makeEqFml :: Fml Component -> Fml Component -> Fml Component
makeEqFml from_ to_ = Or [And[from_, to_], And[Not from_, Not to_]]

notEqP :: Component -> Component -> Fml Component
notEqP from_ to_ = notEqFml (Pred from_) (Pred to_)

notEqFml :: Fml Component -> Fml Component -> Fml Component
notEqFml from_ to_ = Or [And[from_, Not to_], And[Not from_, to_]]

makeEq :: (Int -> Component) -> (Int -> Component) -> Int -> Fml Component
makeEq from_ to_ bitLength = And $ (\bidx -> makeEqP (from_ bidx) (to_ bidx)) <$> [0..bitLength-1]

isZero :: (Int -> Component) -> Int -> Fml Component
isZero type_ bitLength = And $ Not . Pred . type_ <$> [0..bitLength-1]

notZero :: (Int -> Component) -> Int -> Fml Component
notZero type_ bitLength = Or $ Pred . type_ <$> [0..bitLength-1]

genInitState :: Int -> [Int] -> States
genInitState progLen inTape =
                    And [pc, ic, it, mc, oc, mt]
                    where
                      inLenBits = calcBitLength (length inTape)
                      progLenBits = calcBitLength progLen
                      pc = isZero (PC t0) progLenBits
                      ic = isZero (IC t0) inLenBits
                      mc = isZero (MC t0) tapeLenBits
                      oc = isZero (OC t0) outLenBits
                      it = And $ (\(idx, v) -> makeConst (InTape idx) valueBits v) <$> zip [0..] inTape
                      mt = And $ (\idx -> isZero (MidTape t0 idx) tapeLenBits) <$> [0..tapeLen-1]

changePC :: Int -> Int -> Time -> PC -> States
changePC progLenBits progLen t nextPC = makeConst (PC t) progLenBits (min progLen nextPC)

makeInc :: (Int -> Component) -> (Int -> Component) -> Int -> [Int] -> Fml Component
makeInc from_ to_ bitLength addr =
  And $ [notEqP (from_ 0) (to_ 0), makeEqP (from_ 0) (Tmp (0:addr))]
          ++ ((\bidx ->
              Or [
                And [      Pred $ Tmp ((bidx-1):addr),  notEqP (from_ bidx) (to_ bidx), makeEqP (from_ bidx) (Tmp (bidx:addr))],
                And [Not $ Pred $ Tmp ((bidx-1):addr), makeEqP (from_ bidx) (to_ bidx), Not $ Pred $ Tmp (bidx:addr)]
              ]) <$> [1..(bitLength-1)])

makeDec :: (Int -> Component) -> (Int -> Component) -> Int -> [Int] -> Fml Component
makeDec from_ to_ bitLength addr =
  And $ [notEqP (from_ 0) (to_ 0), notEqP (from_ 0) (Tmp (0:addr))]
          ++ ((\bidx ->
              Or [
                And [      Pred $ Tmp ((bidx-1):addr),  notEqP (from_ bidx) (to_ bidx), notEqP (from_ bidx) (Tmp (bidx:addr))],
                And [Not $ Pred $ Tmp ((bidx-1):addr), makeEqP (from_ bidx) (to_ bidx), Not $ Pred $ Tmp (bidx:addr)]
              ]) <$> [1..bitLength-1])

keepMidTape :: Time -> Time -> States
keepMidTape from to = And $ (\mc -> makeEq (MidTape from mc) (MidTape to mc) tapeLenBits) <$> [0..(tapeLen-1)]

keepMidTapeElse :: Time -> Time -> Int -> States
keepMidTapeElse from to notUsed = And $ (\mc -> makeEq (MidTape from mc) (MidTape to mc) tapeLenBits) <$> filter (/= notUsed) [0..(tapeLen-1)]

incMidTape :: Time -> Time -> [Int] -> States
incMidTape from to addr = Or $ fmap each [0..tapeLen-1]
  where
    each idx = And [isConst (MC from) tapeLenBits idx,makeInc (MidTape from idx) (MidTape to idx) tapeLenBits addr, keepMidTapeElse from to idx]

decMidTape :: Time -> Time -> [Int] -> States
decMidTape from to addr = Or $ fmap each [0..tapeLen-1]
  where
    each idx = And [isConst (MC from) tapeLenBits idx,makeDec (MidTape from idx) (MidTape to idx) tapeLenBits addr, keepMidTapeElse from to idx]

--
keepSC :: Int -> (Time->Int->Component) -> Time -> Time -> States
keepSC bitLength cons from to = makeEq (cons from) (cons to) bitLength

keepOC :: Time -> Time -> States
keepOC = keepSC outLenBits OC

keepIC :: Int -> Time -> Time -> States
keepIC inLenBits = keepSC inLenBits IC

keepMC :: Time -> Time -> States
keepMC = keepSC tapeLenBits MC

keepPC :: Int -> Time -> Time -> States
keepPC pcLenBits = keepSC pcLenBits PC

incIC :: Int -> Time -> Time -> [Int] -> States
incIC inLenBits from to = makeInc (IC from) (IC to) inLenBits
incOC :: Time -> Time -> [Int] -> States
incOC from to = makeInc (OC from) (OC to) outLenBits
incMC :: Time -> Time -> [Int] -> States
incMC from to = makeInc (OC from) (OC to) tapeLenBits
decMC :: Time -> Time -> [Int] -> States
decMC from to = makeInc (MC from) (MC to) tapeLenBits

readInput :: Int -> Int -> Time -> Time -> States
readInput inLen inLenBits from to = Or $ fmap (\(mi,ii) -> And [isConst (MC from) tapeLenBits mi, makeConst (IC to) inLenBits ii, keepMidTapeElse from to mi, makeEq (MidTape to mi) (InTape ii) valueBits]) (prod [0..(tapeLen-1)] [0..(inLen-1)])

printOutput :: Time -> States
printOutput from = Or $ fmap (\(mi,oi) -> And [isConst (MC from) tapeLenBits mi, isConst (OC from) outLenBits oi, makeEq (MidTape from mi) (OutTape oi) valueBits]) (prod [0..(tapeLen-1)] [0..(outLen-1)])

acceptRule :: Int -> Int -> Time -> States
acceptRule progLen inLength t =
  And [nowPc, keepedPC, keepedMem, keepedMC, keepedOC, keepedIC]
  where
    from = t
    to = incTime t
    inLenBits = calcBitLength inLength
    progLenBits = calcBitLength progLen
    nowPc = isConst (PC t) progLenBits progLen
    keepedPC = keepPC progLenBits from to
    keepedMem = keepMidTape from to
    keepedOC = keepOC from to
    keepedMC = keepMC from to
    keepedIC = keepIC inLenBits from to

genOpRule :: Int -> Int -> Time -> PC -> Tree -> States
genOpRule progLen inLength t pc op =
  case op of
    P.PtInc          -> And [nowPc, incPCp, keepedMem,           keepedOC, keepedIC, incMC from to addr]
    P.PtDec          -> And [nowPc, incPCp, keepedMem,           keepedOC, keepedIC, decMC from to addr]
    P.ValInc         -> And [nowPc, incPCp,            keepedMC, keepedOC, keepedIC, incMidTape from to addr]
    P.ValDec         -> And [nowPc, incPCp,            keepedMC, keepedOC, keepedIC, decMidTape from to addr]
    P.PutC           -> And [nowPc, incPCp, keepedMem, keepedMC,           keepedIC, printOutput from, incOC from to addr]
    P.GetC           -> And [nowPc, incPCp,            keepedMC, keepedOC,           readInput inLength inLenBits from to, incIC inLenBits from to addr]
    P.LoopBegin next -> And [nowPc,         keepedMem, keepedMC, keepedOC, keepedIC, Or $ map (\mc -> And [isConst (MC t) tapeLenBits mc, Or [And[ isZero (MidTape from mc) valueBits, changePC progLenBits progLen to next], And[notZero (MidTape from mc) valueBits, incPCp]]]) [0..tapeLen-1]]
    P.LoopEnd next   -> And [nowPc,         keepedMem, keepedMC, keepedOC, keepedIC, Or $ map (\mc -> And [isConst (MC t) tapeLenBits mc, Or [And[notZero (MidTape from mc) valueBits, changePC progLenBits progLen to next], And[ isZero (MidTape from mc) valueBits, incPCp]]]) [0..tapeLen-1]]
  where
    addr = [getTime t,0]
    inLenBits = calcBitLength inLength
    progLenBits = calcBitLength progLen
    from = t
    to = incTime t
    nowPc = isConst (PC t) progLenBits pc
    incPCp = changePC progLenBits progLen to (incPc pc)
    keepedMem = keepMidTape from to
    keepedOC = keepOC from to
    keepedMC = keepMC from to
    keepedIC = keepIC inLenBits from to

genStepRules :: Time -> [P.Tree] -> Int -> States
genStepRules t src inLength = Or $ acceptRule (length src) inLength t:fmap (uncurry $ genOpRule (length src) inLength t) (zip [0..] src)

genMiddleState :: [P.Tree] -> Int -> States
genMiddleState src inLength = And $ fmap ((\t -> genStepRules t src inLength) . Time) [(getTime t0)..(getTime lastTime - 1)]

--------------------------------------------------------------------------------

zeroFillOut :: Int -> States
zeroFillOut idx = isZero (OutTape idx) valueBits

greaterThanOC :: Time -> Int -> States
greaterThanOC t idx = Or $ isConst (OC t) outLenBits <$> [idx+1..(outLen-1)]

flushOC :: Time -> States
flushOC t = And $ fmap (\ idx -> Or [greaterThanOC t idx, zeroFillOut idx]) [0..outLen-1]

genLastState :: Int -> States
genLastState progLen = And [isConst (PC lastTime) progLenBits progLen, flushOC lastTime]
                        where
                          progLenBits = calcBitLength progLen
