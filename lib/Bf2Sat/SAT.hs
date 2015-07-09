module Bf2Sat.SAT (Time(..), Component(..), Fml(..), States, gen) where
import Bf2Sat.Parser as P

import qualified Control.Arrow as CA

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

maxValue :: Int
maxValue = 16

t0 :: Time
t0 = Time 0

lastTime :: Time
lastTime = Time 9

tapeLength :: Int
tapeLength = 10

outLength :: Int
outLength = 10

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

genOpRule :: Int -> Time -> PC -> Tree -> States
genOpRule inLength t pc op =
  case op of
    P.PtInc -> And [Pred (PC t pc), Pred (PC (inc t) (incPc pc)), Or $ fmap (\idx -> And [Pred (MC t idx), Pred (MC (inc t) ((idx              + 1)  `mod` tapeLength))]) [0..(tapeLength-1)]]
    P.PtDec -> And [Pred (PC t pc), Pred (PC (inc t) (incPc pc)), Or $ fmap (\idx -> And [Pred (MC t idx), Pred (MC (inc t) ((idx + tapeLength - 1)  `mod` tapeLength))]) [0..(tapeLength-1)]]
    P.ValInc -> And [Pred (PC t pc), Pred (PC (inc t) (incPc pc)), Or $ fmap (\idx -> And [Pred (MC t idx), Or $ fmap (\v -> And [Pred (MidTape t idx v),Pred (MidTape (inc t) idx ((v            + 1) `mod` maxValue))]) [0..(maxValue-1)] ]) [0..(tapeLength-1)]]
    P.ValDec -> And [Pred (PC t pc), Pred (PC (inc t) (incPc pc)), Or $ fmap (\idx -> And [Pred (MC t idx), Or $ fmap (\v -> And [Pred (MidTape t idx v),Pred (MidTape (inc t) idx ((v + maxValue - 1) `mod` maxValue))]) [0..(maxValue-1)] ]) [0..(tapeLength-1)]]
    P.PutC ->  And [Pred (PC t pc), Pred (PC (inc t) (incPc pc)), Or $ fmap (\(mi,oi) -> And $ [Pred (MC t mi), Pred (OC t oi), Or $ fmap (\v -> Or [And [Pred (MidTape t mi v), Pred (OutTape oi v)], And[Not $ Pred (MidTape t mi v), Not $ Pred (OutTape oi v)]]) [0..(maxValue-1)]]) (prod [0..(tapeLength-1)] [0..(outLength-1)]) ]
    P.GetC -> And [Pred (PC t pc), Pred (PC (inc t) (incPc pc)), Or $ fmap (\(mi,ii) -> And $ [Pred (MC t mi), Pred (IC t ii), Or $ fmap (\v -> Or [And [Pred (MidTape t mi v), Pred (InTape ii v)], And[Not $ Pred (MidTape t mi v), Not $ Pred (InTape ii v)]]) [0..(maxValue-1)]]) (prod [0..(tapeLength-1)] [0..(inLength-1)]) ]
    P.LoopBegin next -> And [Pred (PC t pc), Or $ map (\mc -> And [Pred (MC t mc), Or [And[Pred (MidTape t mc 0), Pred (PC (inc t) next)], Pred (PC (inc t) (incPc pc))]]) [0..tapeLength-1]]
    P.LoopEnd next -> And [Pred (PC t pc), Or $ map (\mc -> And [Pred (MC t mc), Or [And[Not (Pred (MidTape t mc 0)), Pred (PC (inc t) next)], Pred (PC (inc t) (incPc pc))]]) [0..tapeLength-1]]

genStepRules :: Time -> [P.Tree] -> Int -> States
genStepRules t src inLength = Or $ And [Pred (PC t (length src)), Pred (PC (inc t) (length src))]:fmap (uncurry $ genOpRule inLength t) (zip [0..] src)

genMiddleState :: [P.Tree] -> Int -> States
genMiddleState src inLength = And $ fmap ((\t -> genStepRules t src inLength) . Time) [(getTime t0)..(getTime lastTime - 1)]

genLastState :: Int -> States
genLastState programLength = Pred $ PC lastTime programLength
