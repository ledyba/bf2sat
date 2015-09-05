module Brainfuck2Sat.SAT (Time(..), Component(..), Fml(..), States, gen) where

import Brainfuck2Sat.Parser (Tree(..), Source(..))
import Brainfuck2Sat.Util (calcBitLength, toBitList, sortOn, showIO)

import Data.List (groupBy)
import Control.Applicative ((<$>))
import Data.Hashable (Hashable, hash, hashWithSalt)
import qualified Control.Arrow as CA

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
  hashWithSalt s (PC t i) = s +　hash t + hash i + 98540371
  hashWithSalt s (IC t i) = s +　hash t + hash i + 48397042
  hashWithSalt s (MC t i) = s + hash t + hash i + 18751233
  hashWithSalt s (OC t i) = s + hash t + hash i + 87510674
  hashWithSalt s (InTape t i) = s + hash t + hash i + 210535
  hashWithSalt s (OutTape t i) = s + hash t + hash i + 12345326
  hashWithSalt s (MidTape t i j) = s + hash t + hash i + hash j + 102345527
  hashWithSalt s (Tmp k) = s + hash k + 30545214358

instance Show Time where
  showsPrec d t = showsPrec d (getTime t)

instance Read Time where
  readsPrec d s = fmap (CA.first Time) (readsPrec d s)

prod :: [a] -> [a] -> [(a,a)]
prod a b = concat $ fmap ( \it1 -> fmap (\it2 -> (it1,it2) ) b ) a

--------------------------------------------------------------------------------

gen :: Source -> States
gen src =
            And [
              genInitState src
              ,genMiddleState src
              ,genLastState src
            ]


t0 :: Time
t0 = Time 0

lastTime :: Source -> Time
lastTime src = Time (getSimStep src - 1)

--------------------------------------------------------------------------------

incTime :: Time -> Time
incTime t = Time $ 1 + getTime t

--------------------------------------------------------------------------------

makeConst :: (Int -> Component) -> Int -> Int -> Fml Component
makeConst type_ bitLength value =
      And $
        fmap (\(bi,b) -> if b then Pred $ type_ bi else Not (Pred (type_ bi)))
        (zip [0..] (toBitList bitLength value))

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

genInitState :: Source -> States
genInitState src =
                    And [pc, ic, it, mc, oc, mt]
                    where
                      inTape = getInTape src
                      tapeLenBits = getAddrBits src
                      outLenBits = getOutAddrBits src
                      valueBits = getValueBits src
                      tapeLen = 2 ^ tapeLenBits
                      inLenBits = calcBitLength (length inTape)
                      progLen = length $ getAST src
                      progLenBits = calcBitLength progLen
                      pc = isZero (PC t0) progLenBits
                      ic = isZero (IC t0) inLenBits
                      mc = isZero (MC t0) tapeLenBits
                      oc = isZero (OC t0) outLenBits
                      it = And $ zip [0..] inTape >>= (\(idx, v) -> if v >= 0 then [makeConst (InTape idx) valueBits v] else [])
                      mt = And $ (\idx -> isZero (MidTape t0 idx) valueBits) <$> [0..(tapeLen-1)]

changePC :: (Int,Int) -> Time -> Time -> [Int] -> States
changePC (progLen,progLenBits) from_ to_ addr_ = Or [ And [isConst (PC from_) progLenBits progLen, makeEq (PC from_) (PC to_) progLenBits], And [Not $ isConst (PC from_) progLenBits progLen, makeInc (PC from_) (PC to_) progLenBits addr_] ]

makeInc :: (Int -> Component) -> (Int -> Component) -> Int -> [Int] -> Fml Component
makeInc from_ to_ bitLength addr =
  And $ And[notEqP (from_ 0) (to_ 0), makeEqP (from_ 0) (Tmp (0:addr))]
          :((\bidx ->
              Or [
                And [      Pred $ Tmp ((bidx-1):addr),  notEqP (from_ bidx) (to_ bidx), makeEqP (from_ bidx) (Tmp $ bidx:addr)],
                And [Not $ Pred $ Tmp ((bidx-1):addr), makeEqP (from_ bidx) (to_ bidx), Not $ Pred $ Tmp $ bidx:addr]
              ]) <$> [1..(bitLength-1)])

makeDec :: (Int -> Component) -> (Int -> Component) -> Int -> [Int] -> Fml Component
makeDec from_ to_ bitLength addr =
  And $ And[notEqP (from_ 0) (to_ 0), notEqP (from_ 0) (Tmp (0:addr))]
          :((\bidx ->
              Or [
                And [      Pred $ Tmp ((bidx-1):addr),  notEqP (from_ bidx) (to_ bidx), notEqP (from_ bidx) (Tmp $ bidx:addr)],
                And [Not $ Pred $ Tmp ((bidx-1):addr), makeEqP (from_ bidx) (to_ bidx), Not $ Pred $ Tmp $ bidx:addr]
              ]) <$> [1..bitLength-1])

keepMidTape :: Int -> Int -> Time -> Time -> States
keepMidTape valueBits tapeLen from to = And $ (\mc -> makeEq (MidTape from mc) (MidTape to mc) valueBits) <$> [0..(tapeLen-1)]

keepMidTapeElse :: Int -> Int -> Time -> Time -> Int -> States
keepMidTapeElse valueBits tapeLen from to notUsed = And $ (\mc -> makeEq (MidTape from mc) (MidTape to mc) valueBits) <$> filter (/= notUsed) [0..(tapeLen-1)]

incMidTape :: Int -> (Int,Int) -> Time -> Time -> [Int] -> States
incMidTape valueBits (tapeLen,tapeLenBits) from to addr = Or $ fmap each [0..tapeLen-1]
  where
    each idx = And [isConst (MC from) tapeLenBits idx,makeInc (MidTape from idx) (MidTape to idx) valueBits addr, keepMidTapeElse valueBits tapeLen from to idx]

decMidTape :: Int -> (Int,Int) -> Time -> Time -> [Int] -> States
decMidTape valueBits (tapeLen,tapeLenBits) from to addr = Or $ fmap each [0..tapeLen-1]
  where
    each idx = And [isConst (MC from) tapeLenBits idx,makeDec (MidTape from idx) (MidTape to idx) valueBits addr, keepMidTapeElse valueBits tapeLen from to idx]

--
keepSC :: Int -> (Time->Int->Component) -> Time -> Time -> States
keepSC bitLength cons from to = makeEq (cons from) (cons to) bitLength

keepOC :: Int -> Time -> Time -> States
keepOC outLenBits = keepSC outLenBits OC

keepIC :: Int -> Time -> Time -> States
keepIC inLenBits = keepSC inLenBits IC

keepMC :: Int -> Time -> Time -> States
keepMC tapeLenBits = keepSC tapeLenBits MC

keepPC :: Int -> Time -> Time -> States
keepPC pcLenBits = keepSC pcLenBits PC

incIC :: Int -> Time -> Time -> [Int] -> States
incIC inLenBits from to = makeInc (IC from) (IC to) inLenBits

incOC :: Int -> Time -> Time -> [Int] -> States
incOC outLenBits from to = makeInc (OC from) (OC to) outLenBits

incMC :: Int -> Time -> Time -> [Int] -> States
incMC tapeLenBits from to = makeInc (MC from) (MC to) tapeLenBits

decMC :: Int -> Time -> Time -> [Int] -> States
decMC tapeLenBits from to = makeDec (MC from) (MC to) tapeLenBits

readInput :: (Int,Int) -> (Int,Int) -> Int -> Time -> Time -> States
readInput (inLen,inLenBits) (tapeLen,tapeLenBits) valueBits from to = Or $ fmap (\(mi,ii) -> And [isConst (MC from) tapeLenBits mi, isConst (IC from) inLenBits ii, keepMidTapeElse valueBits tapeLen from to mi, makeEq (MidTape to mi) (InTape ii) valueBits]) (prod [0..(tapeLen-1)] [0..(inLen-1)])

printOutput :: (Int,Int) -> (Int,Int) -> Int -> Time -> States
printOutput (outLen,outLenBits) (tapeLen,tapeLenBits) valueBits from = Or $ fmap (\(mi,oi) -> And [isConst (MC from) tapeLenBits mi, isConst (OC from) outLenBits oi, makeEq (MidTape from mi) (OutTape oi) valueBits]) (prod [0..(tapeLen-1)] [0..(outLen-1)])

acceptRule :: Source -> Time -> States
acceptRule src t =
  And [nowPc, keepedPC, keepedMem, keepedMC, keepedOC, keepedIC]
  where
    inLen = length (getInTape src)
    inLenBits = calcBitLength inLen
    progLen = length (getAST src)
    progLenBits = calcBitLength progLen
    tapeLenBits = getAddrBits src
    tapeLen = 2 ^ tapeLenBits
    outLenBits = getOutAddrBits src
    valueBits = getValueBits src
    from = t
    to = incTime t
    nowPc = isConst (PC t) progLenBits progLen
    keepedPC = keepPC progLenBits from to
    keepedMem = keepMidTape valueBits tapeLen from to
    keepedOC = keepOC outLenBits from to
    keepedMC = keepMC tapeLenBits from to
    keepedIC = keepIC inLenBits from to

genOpRule :: Source -> Time -> [PC] -> Tree -> [Int] -> States
genOpRule src t pcs op addr =
  case op of
    PtInc          -> And [nowPc, incPCp, keepedMem,           keepedOC, keepedIC, incMC tapeLenBits from to (0:addr)]
    PtDec          -> And [nowPc, incPCp, keepedMem,           keepedOC, keepedIC, decMC tapeLenBits from to (0:addr)]
    ValInc         -> And [nowPc, incPCp,            keepedMC, keepedOC, keepedIC, incMidTape valueBits (tapeLen,tapeLenBits) from to (0:addr)]
    ValDec         -> And [nowPc, incPCp,            keepedMC, keepedOC, keepedIC, decMidTape valueBits (tapeLen,tapeLenBits) from to (0:addr)]
    PutC           -> And [nowPc, incPCp, keepedMem, keepedMC,           keepedIC, printOutput (outLen,outLenBits) (tapeLen,tapeLenBits) valueBits from,    incOC outLenBits from to (0:addr)]
    GetC           -> And [nowPc, incPCp,            keepedMC, keepedOC,           readInput   (inLen,inLenBits)   (tapeLen,tapeLenBits) valueBits from to, incIC inLenBits  from to (0:addr)]
    LoopBegin next -> And [nowPc,         keepedMem, keepedMC, keepedOC, keepedIC, Or $ map (\mc -> And [isConst (MC t) tapeLenBits mc, Or [And[ isZero (MidTape from mc) valueBits, makeConst (PC to) progLenBits next], And[notZero (MidTape from mc) valueBits, incPCp]]]) [0..tapeLen-1]]
    LoopEnd next   -> And [nowPc,         keepedMem, keepedMC, keepedOC, keepedIC, Or $ map (\mc -> And [isConst (MC t) tapeLenBits mc, Or [And[notZero (MidTape from mc) valueBits, makeConst (PC to) progLenBits next], And[ isZero (MidTape from mc) valueBits, incPCp]]]) [0..tapeLen-1]]
  where
    inLen = length (getInTape src)
    inLenBits = calcBitLength inLen
    progLen = length (getAST src)
    progLenBits = calcBitLength progLen
    tapeLenBits = getAddrBits src
    tapeLen = 2 ^ tapeLenBits
    outLenBits = getOutAddrBits src
    outLen = 2 ^ outLenBits
    valueBits = getValueBits src
    from = t
    to = incTime t
    nowPc = Or $ fmap (isConst (PC t) progLenBits) pcs
    incPCp = changePC (progLen,progLenBits) from to (1:addr)
    keepedMem = keepMidTape valueBits tapeLen from to
    keepedOC = keepOC outLenBits from to
    keepedMC = keepMC tapeLenBits from to
    keepedIC = keepIC inLenBits from to

genStepRules :: Time -> Source -> States
genStepRules t src = Or $
              acceptRule src t
              :fmap (\(pc,op) -> genOpRule src t pc op [getTime t,0]) grouped
            where
              grouped = fmap (\lst -> (fmap fst lst,snd $ head lst)) $ groupBy (\(_,op1) (_,op2) -> op1 == op2) $ sortOn (\(_,op)->op) (zip [0..] (getAST src))

genMiddleState :: Source -> States
genMiddleState src = And $ fmap ((\t -> genStepRules t src) . Time) [(getTime t0)..(getTime (lastTime src) - 1)]

--------------------------------------------------------------------------------

zeroFillOut :: Int -> Int -> States
zeroFillOut outLenBits idx = isZero (OutTape idx) outLenBits

greaterThanOC :: (Int,Int) -> Time -> Int -> States
greaterThanOC (outLen, outLenBits) t idx = Or $ isConst (OC t) outLenBits <$> [idx+1..(outLen-1)]

flushOC :: (Int,Int) -> Time -> States
flushOC (outLen, outLenBits) t = And $ fmap (\ idx -> Or [greaterThanOC (outLen,outLenBits) t idx, zeroFillOut outLenBits idx]) [0..outLen-1]

genLastState :: Source -> States
genLastState src = And [isConst (PC $ lastTime src) progLenBits progLen, flushOC (outLen,outLenBits) (lastTime src)]
                        where
                          outLenBits = getOutAddrBits src
                          outLen = 2 ^ outLenBits
                          progLen = length (getAST src)
                          progLenBits = calcBitLength progLen
