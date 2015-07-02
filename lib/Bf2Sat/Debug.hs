module Bf2Sat.Debug (eval) where

import Bf2Sat.SAT
import Bf2Sat.Engine

eval' :: Component -> [Int] -> [ID] -> Bool
eval' (PC t pc) _ ids = getPC (ids !! getTime t) == pc
eval' (IC t ic) _ ids = getIC (ids !! getTime t) == ic
eval' (InTape idx v) inTape _ = (inTape !! idx) == v
eval' (MC t pt) _ ids = getPT (ids !! getTime t) == pt
eval' (MidTape t pt v) _ ids = (getMem (ids !! getTime t) !! pt) == v
eval' (OC t oc) _ ids = getOC (ids !! getTime t) == oc
eval' (OutTape idx v) _ ids = getOut (last ids) !! idx == v

eval :: States -> [Int] -> [ID] -> Bool
eval (Pred x) intape ids = eval' x intape ids
eval (Not x) intape ids = not (eval x intape ids)
eval (And xs) intape ids = foldl (\b x -> b && eval x intape ids) True xs
eval (Or xs) intape ids = foldl (\b x -> b || eval x intape ids) False xs
