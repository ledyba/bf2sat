module Main where

import qualified Brainfuck2Sat.Parser as P
import qualified Brainfuck2Sat.SAT as S
import qualified Brainfuck2Sat.Engine as E
import qualified Brainfuck2Sat.Debug as D
import qualified Brainfuck2Sat.CNF as C
import qualified Brainfuck2Sat.RCNF as R
import qualified Data.HashMap.Strict as M
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
  putStrLn "write to file"
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
  C.toDIMACS isat dict "sat.txt"

check :: FilePath -> IO ()
check fname = withSource fname $ \ src -> do
  let ids = E.run src
  putStrLn "-- Setting --"
  putStrLn $ show src
  preds <- fmap (read :: String -> [(Int, S.Component)]) (readFile "pred.txt")
  ansStr <- readFile "ans.txt"
  let ans = R.fromDIMACS preds ansStr
  let val = D.valuation (M.keys ans) src ids
  putStrLn $ "** Exec " ++ show (length ids) ++ " Steps **"
  let pairs = fmap (\(p2,v) -> (p2,  M.lookupDefault (error "???") p2 ans,v)) val
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
--
exec :: FilePath -> IO ()
exec fname = withSource fname $ \ src -> do
  let ids = E.run src
  putStrLn "-- Setting --"
  putStrLn $ show src
  putStrLn $ "** Exec " ++ show (length ids) ++ " Steps **"
  putStrLn "IDs:"
  putStrLn $ L.intercalate "\n" $ fmap (\(idx, it) -> show idx ++ ": " ++ show it) (zip ([0.. ] :: [Int]) ids)

decode :: FilePath -> IO ()
decode fname = withSource fname $ \ src -> do
  putStrLn "-- Setting --"
  putStrLn $ show src
  preds <- fmap (read :: String -> [(Int, S.Component)]) (readFile "pred.txt")
  ansStr <- readFile "ans.txt"
  let ans = R.fromDIMACS preds ansStr
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
  putStrLn "  cabal run decode <hoge.bf>"
  putStrLn "  cabal run exec <hoge.bf>"

main :: IO ()
main = do
  putStrLn "** Brainfuck 2 SAT **"
  argv <- getArgs
  case argv of
    ("create":fpath:_) -> create fpath
    ("check":fpath:_) -> check fpath
    ("decode":fpath:_) -> decode fpath
    ("exec":fpath:_) -> exec fpath
    _ -> usage
  putStrLn "All done, have fun."
