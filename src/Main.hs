module Main where

import qualified Brainfuck2Sat.Parser as P
import qualified Brainfuck2Sat.SAT as S
import qualified Brainfuck2Sat.Engine as E
import qualified Brainfuck2Sat.Debug as D
import qualified Brainfuck2Sat.CNF as C
import qualified Brainfuck2Sat.RCNF as R
import Brainfuck2Sat.Util

import System.Environment
import Control.Applicative ((<$>))
import qualified Data.List as L

makeCNF :: P.Source -> IO [[C.CFml S.Component]]
makeCNF src = do
  putStrLn "To CNF..."
  let sat = S.gen src
  let cnf = C.toCNF $ C.removeNot sat
  putStrLn $ show (length cnf) ++ " clauses, " ++ show (foldl (\t a -> t + length a) 0 cnf) ++ " literals"
  return cnf

makeSAT :: P.Source -> IO ([[Int]], [(Int, S.Component)])
makeSAT src = do
  cnf <- makeCNF src
  putStrLn "Aliasing..."
  let (isat, dict) = C.makeAlias cnf
  putStrLn $ show (length dict)++" uniq predicates"
  putStrLn "writ to file"
  return (isat,dict)

withSource :: FilePath -> (P.Source -> IO ()) -> IO ()
withSource fpath cont = do
  text <- readFile fpath
  case P.parse fpath text of
    Right src -> cont src
    Left e -> print e

create :: FilePath -> IO()
create fname = withSource fname $ \ src -> do
  putStrLn "-- Setting --"
  putStrLn $ show src
  putStrLn "-- Create SAT problem --"
  (isat, dict) <- makeSAT src
  writeFile "pred.txt" (show dict)
  C.toDMACS isat dict "sat.txt"

check :: FilePath -> IO ()
check fname = withSource fname $ \ src -> do
  let ids = E.run src
  putStrLn "-- Setting --"
  putStrLn $ show src
  preds <- fmap (read :: String -> [(Int, S.Component)]) (readFile "pred.txt")
  ansStr <- readFile "ans.txt"
  let ans = R.fromDMACS preds ansStr
  let val = D.valuation (fmap fst ans) src ids
  print $ "Exec " ++ show (length ids) ++ " Steps"
  let pairs = (\((p1,a),(p2,v)) -> if p1 == p2 then (p1,a,v) else error "???" ) <$> zip ans val
  let notmatched = filter (\(_,a,v) -> a /= v) pairs
  putStrLn $ "Do not match: " ++ show (length notmatched) ++ " predicates"
  mapM_ (\(cmp, a, v) -> putStrLn $ "(" ++ show cmp ++ ") / actual: "++ show a ++ " expected: "++ show v) notmatched
  putStrLn "-- Result --"
  let (intape, rids) = E.fromSAT src ans
  putStrLn $ "         Source:" ++ P.getSource src
  putStrLn $ "          Input: " ++ showInTape (P.getInTape src)
  putStrLn $ "Estimated Input: " ++ showInTape intape
  putStrLn "Estimated IDs:"
  putStrLn $ L.intercalate "\n" $ fmap (\(idx, it) -> show idx ++ ": " ++ show it) (zip ([0.. ] :: [Int]) rids)

recover :: FilePath -> IO ()
recover fname = withSource fname $ \ src -> do
  putStrLn "-- Setting --"
  putStrLn $ show src
  preds <- fmap (read :: String -> [(Int, S.Component)]) (readFile "pred.txt")
  ansStr <- readFile "ans.txt"
  let ans = R.fromDMACS preds ansStr
  let (intape, rids) = E.fromSAT src ans
  putStrLn "-- Result --"
  putStrLn $ "         Source:" ++ P.getSource src
  putStrLn $ "          Input: " ++ showInTape (P.getInTape src)
  putStrLn $ "Estimated Input: " ++ showInTape intape
  putStrLn "Estimated IDs:"
  putStrLn $ L.intercalate "\n" $ fmap (\(idx, it) -> show idx ++ ": " ++ show it) (zip ([0.. ] :: [Int]) rids)

usage :: IO()
usage = do
  putStrLn "usage:"
  putStrLn "  cabal run create <hoge.bf>"
  putStrLn "  cabal run check <hoge.bf>"
  putStrLn "  cabal run recover <hoge.bf>"

main :: IO ()
main = do
  putStrLn "** Brainfuck 2 SAT **"
  argv <- getArgs
  case argv of
    ("create":fpath:_) -> create fpath
    ("check":fpath:_) -> check fpath
    ("recover":fpath:_) -> recover fpath
    _ -> usage
  putStrLn "All done, have fun."
