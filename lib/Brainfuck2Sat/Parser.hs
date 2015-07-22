module Brainfuck2Sat.Parser (parse, Source(..), Tree(..)) where

import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

data Source = Source {
  getAST :: [Tree],
  getInTape :: [Int],
  getValueBits :: Int,
  getAddrBits :: Int,
  getOutAddrBits :: Int,
  getSimStep :: Int
}
data Tree = PtInc | PtDec | ValInc | ValDec | PutC | GetC | LoopBegin Int | LoopEnd Int deriving (Show, Eq)

instance Show Source where
  show (Source ast intape valueBits addrBits outAddrBits simSteps) =
        "----\n  src:" ++ show ast ++
        "\n  in: " ++ show intape ++
        "\n  value-bits: " ++ show valueBits ++
        "\n  addr-bits:" ++ show addrBits ++
        "\n  out-addr-bits:" ++ show outAddrBits ++
        "\n  sim-steps:" ++ show simSteps ++
        "\n----"

flattenList :: [[a']] -> [a']
flattenList = concat

parse :: FilePath -> String -> Either ParseError Source
parse = P.parse parseBody

parseBody :: Parser Source
parseBody = parseBody' $ Source [] [] 8 4 4 4
  where
    parseBody' src = choice [readHead src, readSource src]
    readHead src = do
      next <- choice [readInTape src, readValueBits src, readOutAddrBits src, readAddrBits src, readSimSteps src]
      parseBody' next
    readSource src = do
      P.spaces
      ast <- brainfuck
      return $ Source ast (getInTape src) (getValueBits src) (getAddrBits src) (getOutAddrBits src) (getSimStep src)

readInTape :: Source -> Parser Source
readInTape src = do
  v <- readL "in:" "[]-., 0123456789abcdefABCDEFXx"
  return $ Source (getAST src) (read v) (getValueBits src) (getAddrBits src) (getOutAddrBits src) (getSimStep src)

readValueBits :: Source -> Parser Source
readValueBits src = do
  v <- readL "value-bits:" "0123456789"
  return $ Source (getAST src) (getInTape src) (read v) (getAddrBits src) (getOutAddrBits src) (getSimStep src)

readOutAddrBits :: Source -> Parser Source
readOutAddrBits src = do
  v <- readL "out-addr-bits:" "0123456789"
  return $ Source (getAST src) (getInTape src) (getValueBits src) (getAddrBits src) (read v) (getSimStep src)

readAddrBits :: Source -> Parser Source
readAddrBits src = do
  v <- readL "addr-bits:" "0123456789"
  return $ Source (getAST src) (getInTape src) (getValueBits src) (read v) (getOutAddrBits src) (getSimStep src)

readSimSteps :: Source -> Parser Source
readSimSteps src = do
  v <- readL "steps:" "0123456789"
  return $ Source (getAST src) (getInTape src) (getValueBits src) (getAddrBits src) (getOutAddrBits src) (read v)

readL :: String -> String -> Parser String
readL name vs = do
  P.spaces
  _ <- P.string name
  P.spaces
  tape <- P.many1 (P.oneOf vs)
  P.spaces
  return tape

--------------------------------------------------------------------------------

brainfuck :: Parser [Tree]
brainfuck = fmap (conv . flattenList) (P.many (P.choice [ops, loop]))

conv :: [Tree] -> [Tree]
conv = conv' 0 []

conv' :: Int -> [Tree] -> [Tree] -> [Tree]
conv' idx acc (x:left) = conv' (idx+1) (fixIdx idx x:acc) left
conv' _ acc [] = reverse acc

fixIdx :: Int -> Tree -> Tree
fixIdx idx (LoopBegin d) = LoopBegin (d + idx + 1 + 1)
fixIdx idx (LoopEnd d) = LoopEnd (idx - d - 1 + 1)
fixIdx _ x = x

op :: P.Parser Tree
op = do
   P.spaces
   ch <- P.oneOf "><+-.,"
   P.spaces
   return $ ch2tree ch

ops :: P.Parser [Tree]
ops = P.many1 op

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
