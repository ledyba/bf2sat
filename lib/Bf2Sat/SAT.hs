module Bf2Sat.SAT where

data Fml a = And [Fml a] | Or [Fml a] | Pred a
