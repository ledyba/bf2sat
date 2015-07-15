module Main where

import Brainfuck2Sat.Parser as P
import Brainfuck2Sat.SAT as S
import Brainfuck2Sat.Engine as E
import Brainfuck2Sat.Debug as D
import Brainfuck2Sat.CNF as C
import Brainfuck2Sat.RCNF as R

import System.Environment
import Control.Applicative ((<$>))
import qualified Data.List as L

helloWorld :: String
helloWorld = "+++++ +++[- >++++ ++++< ]>+++ +++++ .---. +++++ ++..+ ++.<"

easyloop :: String
easyloop = "++[-]"

intape :: [Int]
intape = [0,0,0,0]

src :: String
src = helloWorld

create :: IO()
create = do
  putStrLn "Parsing..."
  let Right ast = P.parse src
  putStrLn "To CNF..."
  let cnf = C.toCNF $ C.removeNot $ S.gen ast intape
  putStrLn $ show (length cnf) ++ " clauses, " ++ show (foldl (\t a -> t + length (a)) 0 cnf) ++ " literals"
  putStrLn "Aliasing..."
  let (isat, dict) = C.alias cnf
  putStrLn $ (show $ length dict)++" uniq predicates"
  putStrLn "writ to file"
  writeFile "pred.txt" (show dict)
  C.toDMACS isat dict "sat.txt"
  putStrLn "All done, have fun."

check :: IO ()
check = do
  let Right ast = P.parse src
  let ids = E.run ast intape S.tapeLen S.timeLen
  preds <- fmap (read :: String -> [(Int, Component)]) (readFile "pred.txt")
  ansStr <- readFile "ans.txt"
  let ans = R.fromDMACS preds ansStr
  let val = D.valuation (fmap fst ans) intape ids
  print $ "Exec " ++ show (length ids) ++ " Steps"
  let pairs = (\((p1,a),(p2,v)) -> if p1 == p2 then (p1,a,v) else error "???" ) <$> zip ans val
  let notmatched = filter (\(_,a,v) -> a /= v) pairs
  putStrLn $ "Do not match: " ++ show (length notmatched) ++ " items"
  mapM_ (\(cmp, a, v) -> putStrLn $ "(" ++ show cmp ++ ") / actual: "++ show a ++ " expected: "++ show v) notmatched

test :: IO ()
test = do
  let Right ast = P.parse src
  let ids = E.run ast intape S.tapeLen S.timeLen
  let sat = S.gen ast intape
  let r = D.eval  sat intape ids
  print $ show src
  putStrLn $ L.intercalate "\n" $ fmap (\(idx, it) -> show idx ++ ": " ++ show it) (zip ([0.. ] :: [Int]) ids)
  print $ show sat
  print $ show r

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ("create":_) -> create
    ("check":_) -> check
    ("test":_) -> test
    _ -> print "(>_<)"
