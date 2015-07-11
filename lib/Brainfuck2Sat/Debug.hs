module Brainfuck2Sat.Debug (eval, valuation) where

import Brainfuck2Sat.SAT
import Brainfuck2Sat.Engine

eval' :: Component -> [Int] -> [ID] -> Bool
eval' (PC t pc) _ ids = getPC (ids !! getTime t) == pc
eval' (IC t ic) _ ids = getIC (ids !! getTime t) == ic
eval' (InTape idx v) inTape _ = (inTape !! idx) == v
eval' (MC t pt) _ ids = getPT (ids !! getTime t) == pt
eval' (MidTape t pt v) _ ids = (getMem (ids !! getTime t) !! pt) == v
eval' (OC t oc) _ ids = getOC (ids !! getTime t) == oc
eval' (OutTape idx v) _ ids = if idx < length tape then (tape !! idx) == v
                              else v == 0
                              where tape = getOut (last ids)
eval' (Tmp _) _ _ = error "you can't eval temporary value"

eval :: States -> [Int] -> [ID] -> Bool
eval (Pred x) intape ids = eval' x intape ids
eval (Not x) intape ids = not (eval x intape ids)
eval (And xs) intape ids = foldl (\b x -> b && eval x intape ids) True xs
eval (Or xs) intape ids = foldl (\b x -> b || eval x intape ids) False xs

valuation :: [Component] -> [Int] -> [ID] -> [(Component, Bool)]
valuation comps intape ids = fmap (\comp -> (comp, eval' comp intape ids)) comps
