module Brainfuck2Sat.Debug (eval, valuation) where

import Brainfuck2Sat.SAT
import Brainfuck2Sat.Parser (Source(..))
import Brainfuck2Sat.Engine
import Data.Bits

eval' :: Component -> Source -> [ID] -> Bool
eval' (PC t pc) _ ids = testBit (getPC (ids !! getTime t)) pc
eval' (IC t ic) _ ids = testBit (getIC (ids !! getTime t)) ic
eval' (InTape idx v) src _ = (in_v < 0) || testBit in_v v
                            where
                              inTape = getInTape src
                              in_v = inTape !! idx
eval' (MC t pt) _ ids = testBit (getPT (ids !! getTime t)) pt
eval' (MidTape t pt v) _ ids = testBit (getMem (ids !! getTime t) !! pt) v
eval' (OC t oc) _ ids = testBit (getOC (ids !! getTime t)) oc
eval' (OutTape idx v) _ ids = (idx < length tape) && testBit (tape !! idx) v
                              where tape = getOut (last ids)
eval' (Tmp _) _ _ = error "you can't eval temporary value"

eval :: States -> Source -> [ID] -> Bool
eval (Pred x) src ids = eval' x src ids
eval (Not x) src ids = not (eval x src ids)
eval (And xs) src ids = foldl (\b x -> b && eval x src ids) True xs
eval (Or xs) src ids = foldl (\b x -> b || eval x src ids) False xs

valuation :: [Component] -> Source -> [ID] -> [(Component, Bool)]
valuation comps src ids = fmap (\comp -> (comp, eval' comp src ids)) comps
