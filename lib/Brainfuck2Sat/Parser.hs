module Brainfuck2Sat.Parser (parse, Source(..), Tree(..)) where

import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P
import Brainfuck2Sat.Util (showInTape)
import Data.List (elemIndex, scanl)
import Text.Parsec.Pos (updatePosChar, initialPos)
import Data.Char (isSpace)

data Source = Source {
  getSource :: String,
  getAST :: [Tree],
  getInTape :: [Int],
  getValueBits :: Int,
  getAddrBits :: Int,
  getOutAddrBits :: Int,
  getSimStep :: Int
}
data Tree = PtInc | PtDec | ValInc | ValDec | PutC | GetC | LoopBegin Int | LoopEnd Int deriving (Show, Eq, Ord)

instance Show Source where
  show (Source src ast intape valueBits addrBits outAddrBits simSteps) =
          "  src:" ++ src ++
        "\n  ast:" ++ show ast ++
        "\n  in: " ++ showInTape intape ++
        "\n  value-bits: " ++ show valueBits ++
        "\n  addr-bits:" ++ show addrBits ++
        "\n  out-addr-bits:" ++ show outAddrBits ++
        "\n  sim-steps:" ++ show simSteps

flattenList :: [[a']] -> [a']
flattenList = concat

parse :: FilePath -> String -> Either ParseError Source
parse fpath text = P.parse (parseBody text) fpath text

offset :: String -> SourcePos -> Maybe Int
offset source pos = elemIndex pos positions
  where positions = scanl updatePosChar firstPos source
        firstPos = initialPos (sourceName pos)

strip :: String -> String
strip text = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace text

parseBody :: String -> Parser Source
parseBody text = parseBody' $ Source "" [] [] 8 4 4 4
  where
    parseBody' src = choice [readHead src, readSource src]
    readHead src = do
      next <- choice [readInTape src, readValueBits src, readOutAddrBits src, readAddrBits src, readSimSteps src]
      parseBody' next
    readSource src = do
      P.spaces
      from <- P.getPosition
      ast <- brainfuck
      to <- P.getPosition
      let Just floc = offset text from
      let Just tloc = offset text to
      return $ src {getSource=strip $ drop floc $ take tloc text,getAST=ast }

readInTape :: Source -> Parser Source
readInTape src = do
  v <- readL "in:" "[]-., 0123456789abcdefABCDEFXx"
  return $ src {getInTape=read v}

readValueBits :: Source -> Parser Source
readValueBits src = do
  v <- readL "value-bits:" "0123456789"
  return $ src{ getValueBits= (read v) }

readOutAddrBits :: Source -> Parser Source
readOutAddrBits src = do
  v <- readL "out-addr-bits:" "0123456789"
  return $ src{ getOutAddrBits=(read v) }

readAddrBits :: Source -> Parser Source
readAddrBits src = do
  v <- readL "addr-bits:" "0123456789"
  return $ src{ getAddrBits=(read v) }

readSimSteps :: Source -> Parser Source
readSimSteps src = do
  v <- readL "steps:" "0123456789"
  return $ src{ getSimStep=(read v) }

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
brainfuck = fmap (conv . flattenList) (P.many1 ops)

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
ops = fmap flattenList (P.many1 $ P.choice [loop, P.many1 op])

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
