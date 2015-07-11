module Bf2Sat.SAT (Time(..), Component(..), Fml(..), States, gen, maxValue, outLength, tapeLength, timeLength) where
import Bf2Sat.Parser as P

import qualified Control.Arrow as CA

maxValue :: Int
maxValue = 128
timeLength :: Int
timeLength = 190
tapeLength :: Int
tapeLength = 3
outLength :: Int
outLength = 6

newtype Time = Time {getTime :: Int}  deriving (Eq)
data Component = PC Time Int | IC Time Int | InTape Int Int | MC Time Int | MidTape Time Int Int | OC Time Int | OutTape Int Int | Tmp [Int] deriving (Eq,Show,Read)
data Fml a = And [Fml a] | Or [Fml a] | Not (Fml a) | Pred a deriving (Show, Read, Eq)
type States = Fml Component
type PC = Int

gen :: [P.Tree] -> [Int] -> States
gen src inTape =
            And [genInitState progLen inTape, genLastState progLen, genMiddleState src inTapeLength]
            where
              progLen = length src
              inTapeLength = length inTape

instance Show Time where
  showsPrec d t = showsPrec d (getTime t)

instance Read Time where
  readsPrec d s = fmap (CA.first Time) (readsPrec d s)

t0 :: Time
t0 = Time 0


lastTime :: Time
lastTime = Time (timeLength-1)

inc :: Time -> Time
inc t = Time $ 1 + getTime t

incPc :: PC -> PC
incPc t = t + 1

genInitState :: Int -> [Int] -> States
genInitState programLength inTape =
                    And (pc ++ ic ++ it ++ mc ++ oc ++ mt)
                    where
                      pc = Pred (PC t0 0):fmap (\i -> Not $ Pred $ PC t0 i) [1..(programLength)]
                      ic = Pred (IC t0 0):fmap (\i -> Not $ Pred $ IC t0 i) [1..(length inTape - 1)]
                      mc = Pred (MC t0 0):fmap (\i -> Not $ Pred $ MC t0 i) [1..(tapeLength-1)]
                      oc = Pred (OC t0 0):fmap (\i -> Not $ Pred $ OC t0 i) [1..(outLength-1)]
                      it = foldl mappend [] $ fmap (\(i,k) -> (fmap (\v -> if v == k then Pred$InTape i v else Not $ Pred$InTape i v) [0..maxValue-1])) (zip [0..] inTape)
                      mt = foldl mappend [] $ fmap (\t -> (Pred $ MidTape t0 t 0):fmap (\i -> Not $ Pred $ MidTape t0 t i) [1..(maxValue-1)]) [0..tapeLength-1]

prod :: [a] -> [a] -> [(a,a)]
prod a b = concat $ fmap ( \it1 -> fmap (\it2 -> (it1,it2) ) b ) a

changePC :: Int -> Time -> PC -> States
changePC programLength t nextPC =
  And $ fmap (\pc -> if pc == (min nextPC programLength) then Pred (PC t pc) else Not $ Pred $ PC t pc) [0..programLength]

keepMidTape' :: [Int] -> Time -> Time -> States
keepMidTape' targets from to = And $ fmap (\(val,idx)-> Or [And [Pred (MidTape from idx val), Pred (MidTape to idx val)], And [Not (Pred (MidTape from idx val)), Not (Pred (MidTape to idx val))]]) (prod [0..(maxValue-1)] targets)

keepMidTape :: Time -> Time -> States
keepMidTape = keepMidTape' [0..(tapeLength-1)]

keepMidTapeElse :: Time -> Time -> Int -> States
keepMidTapeElse from to notUsed = keepMidTape' (filter (/= notUsed) [0..(tapeLength-1)]) from to

fixMidTape :: Time -> Time -> Int -> States
fixMidTape from to delta = Or $ fmap eachIndex [0..(tapeLength-1)]
    where
      eachIndex idx = And [Pred (MC from idx), Or (runWithCurrentValue idx), keepMidTapeElse from to idx]
      runWithCurrentValue idx = fmap (eachIndexValue idx) [0..maxValue-1]
      eachIndexValue idx v = And $ Pred (MidTape from idx v) : genNextValues idx v
      genNextValues idx k = fmap (genDeltaValueMap idx k) [0..maxValue-1]
      genDeltaValueMap idx k v = if v == ((k + maxValue + delta) `mod` maxValue)
                                    then Pred (MidTape to idx v)
                                    else Not (Pred (MidTape to idx v))
--
keepSC :: Int -> (Time->Int->Component) -> Time -> Time -> States
keepSC listLength cons from to = And $ fmap (\idx-> Or [And [Pred (cons from idx), Pred (cons to idx)], And [Not (Pred (cons from idx)), Not (Pred (cons to idx))]]) [0..listLength-1]

keepOC :: Time -> Time -> States
keepOC = keepSC tapeLength OC

keepIC :: Int -> Time -> Time -> States
keepIC inLength = keepSC inLength IC

keepMC :: Time -> Time -> States
keepMC = keepSC tapeLength MC

keepPC :: Int -> Time -> Time -> States
keepPC programLength = keepSC programLength PC

incSC :: Int -> (Time->Int->Component) -> Int -> Time -> Time -> States
incSC counterLength cons delta from to = Or $ fmap eachValue [0..(counterLength - 1)]
              where
                eachValue v = And $ Pred (cons from v) : fmap (nextV ((v + delta + counterLength) `mod` counterLength)) [0..counterLength-1]
                nextV tv v = if tv == v then Pred (cons to v) else Not (Pred (cons to v))

incIC :: Int -> Time -> Time -> States
incIC inLength = incSC inLength IC 1
incOC :: Time -> Time -> States
incOC = incSC outLength OC 1
incMC :: Time -> Time -> States
incMC = incSC (tapeLength-1) MC 1
decMC :: Time -> Time -> States
decMC = incSC (tapeLength-1) MC 1

readInput :: Int -> Time -> Time -> States
readInput inLength from to = Or $ fmap (\(mi,ii) -> And $ [Pred (MC from mi), Pred (IC to ii), keepMidTapeElse from to mi, Or $ fmap (\v -> Or [And [Pred (MidTape to mi v), Pred (InTape ii v)], And[Not $ Pred (MidTape to mi v), Not $ Pred (InTape ii v)]]) [0..(maxValue-1)]]) (prod [0..(tapeLength-1)] [0..(inLength-1)])

printOutput :: Time -> States
printOutput from = Or $ fmap (\(mi,oi) -> And $ [Pred (MC from mi), Pred (OC from oi), Or $ fmap (\v -> Or [And [Pred (MidTape from mi v), Pred (OutTape oi v)], And[Not $ Pred (MidTape from mi v), Not $ Pred (OutTape oi v)]]) [0..(maxValue-1)]]) (prod [0..(tapeLength-1)] [0..(outLength-1)])

acceptRule :: Int -> Int -> Time -> States
acceptRule programLength inLength t =
  And [nowPc, keepedPC, keepedMem, keepedMC, keepedOC, keepedIC]
  where
    from = t
    to = inc t
    nowPc = Pred (PC t programLength)
    keepedPC = keepPC programLength from to
    keepedMem = keepMidTape from to
    keepedOC = keepOC from to
    keepedMC = keepMC from to
    keepedIC = keepIC inLength from to

genOpRule :: Int -> Int -> Time -> PC -> Tree -> States
genOpRule programLength inLength t pc op =
  case op of
    P.PtInc          -> And [nowPc, incPCp, keepedMem,           keepedOC, keepedIC, incMC from to]
    P.PtDec          -> And [nowPc, incPCp, keepedMem,           keepedOC, keepedIC, decMC from to]
    P.ValInc         -> And [nowPc, incPCp,            keepedMC, keepedOC, keepedIC, fixMidTape from to 1]
    P.ValDec         -> And [nowPc, incPCp,            keepedMC, keepedOC, keepedIC, fixMidTape from to (-1)]
    P.PutC           -> And [nowPc, incPCp, keepedMem, keepedMC,           keepedIC, printOutput from, incOC from to ]
    P.GetC           -> And [nowPc, incPCp,            keepedMC, keepedOC,           readInput inLength from to, incIC inLength from to]
    P.LoopBegin next -> And [nowPc,         keepedMem, keepedMC, keepedOC, keepedIC, Or $ map (\mc -> And [Pred (MC t mc), Or [And[     Pred (MidTape from mc 0) , changePC programLength to next], And[Not (Pred (MidTape from mc 0)), incPCp]]]) [0..tapeLength-1]]
    P.LoopEnd next   -> And [nowPc,         keepedMem, keepedMC, keepedOC, keepedIC, Or $ map (\mc -> And [Pred (MC t mc), Or [And[Not (Pred (MidTape from mc 0)), changePC programLength to next], And[     Pred (MidTape from mc 0) , incPCp]]]) [0..tapeLength-1]]
  where
    from = t
    to = inc t
    nowPc = Pred (PC t pc)
    incPCp = changePC programLength to (incPc pc)
    keepedMem = keepMidTape from to
    keepedOC = keepOC from to
    keepedMC = keepMC from to
    keepedIC = keepIC inLength from to

genStepRules :: Time -> [P.Tree] -> Int -> States
genStepRules t src inLength = Or $ acceptRule (length src) inLength t:fmap (uncurry $ genOpRule (length src) inLength t) (zip [0..] src)

genMiddleState :: [P.Tree] -> Int -> States
genMiddleState src inLength = And $ fmap ((\t -> genStepRules t src inLength) . Time) [(getTime t0)..(getTime lastTime - 1)]

zeroFillOut :: Int -> States
zeroFillOut idx = And $ (Pred $ OutTape idx 0):fmap (\k -> Not $ Pred $ OutTape idx k) [1..maxValue-1]

greaterThanOC :: Time -> Int -> States
greaterThanOC t idx = Or $ fmap (\k -> Pred $ OC t k) [idx+1..(outLength-1)]

flushOC :: Time -> States
flushOC t = And $ fmap (\ idx -> Or [greaterThanOC t idx, zeroFillOut idx]) [0..outLength-1]

genLastState :: Int -> States
genLastState programLength = And $ [Pred $ PC lastTime programLength, flushOC lastTime]
