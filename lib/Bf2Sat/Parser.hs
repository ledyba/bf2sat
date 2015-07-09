module Bf2Sat.Parser (parse, Tree(..)) where

import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

data Tree = PtInc | PtDec | ValInc | ValDec | PutC | GetC | LoopBegin Int | LoopEnd Int deriving (Show, Eq)

flattenList :: [[a']] -> [a']
flattenList = concat

parse :: String -> Either ParseError [Tree]
parse = P.parse (fmap (conv . flattenList) (P.many (P.choice [ops, loop]))) "<TEXT>"

conv :: [Tree] -> [Tree]
conv = conv' 0 []

conv' :: Int -> [Tree] -> [Tree] -> [Tree]
conv' idx acc (x:left) = conv' (idx+1) (fixIdx idx x:acc) left
conv' _ acc [] = reverse acc

fixIdx :: Int -> Tree -> Tree
fixIdx idx (LoopBegin d) = LoopBegin (d + idx + 1 + 1)
fixIdx idx (LoopEnd d) = LoopEnd (idx - d - 1 + 1)
fixIdx _ x = x

ops :: P.Parser [Tree]
ops = do
    P.spaces
    chs <- P.many1 $ P.oneOf "><+-.,"
    P.spaces
    return $ fmap ch2tree chs

ch2tree :: Char -> Tree
ch2tree ch =
    case ch of
      '>' -> PtInc
      '<' -> PtDec
      '+' -> ValInc
      '-' -> ValDec
      '.' -> PutC
      ',' -> GetC
      _ -> error "???"

loop :: P.Parser [Tree]
loop = do
    P.spaces
    _ <- P.char '['
    chr1 <- P.optionMaybe ops
    _ <- P.char ']'
    P.spaces
    return $ case chr1 of
        Just chr -> [LoopBegin (length chr)] ++ chr ++ [LoopEnd (length chr)]
        Nothing -> [LoopBegin 0,LoopEnd 0]
