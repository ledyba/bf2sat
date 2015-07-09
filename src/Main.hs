import Bf2Sat.Parser as P
import Bf2Sat.SAT as S
import Bf2Sat.Engine as E
import Bf2Sat.Debug as D
import Bf2Sat.CNF as C
import Bf2Sat.RCNF as R

import System.Environment
import qualified Data.List as L

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    ("create":_) -> do
      writeFile "sat.txt" (C.toDMACS isat dict)
      writeFile "pred.txt" (show dict)
    ("check":_) -> do
      preds <- fmap (read :: String -> [(Int, Component)]) (readFile "pred.txt")
      ansStr <- readFile "ans.txt"
      ans <- return $ R.fromDMACS preds ansStr
      val <- return $ D.valuation (fmap fst ans) intape ids
      print $ "Exec " ++ show (length ids) ++ " Steps"
      pairs <- return $  fmap (\((p1,a),(p2,v)) -> if p1 == p2 then (p1,a,v) else error "???" ) $ zip ans val
      notmatched <- return $ filter (\(_,a,v) -> a /= v) pairs
      print $ "Do not match: " ++ (show $ length notmatched) ++ " items"
      print notmatched
    ("test":_) -> do
      print $ show src
      putStrLn $ L.intercalate "\n" $ fmap (\(idx, it) -> (show idx) ++ ": " ++ (show it)) (zip [0..] ids)
      -- print $ show sat
      print $ show r
    _ -> print "(>_<)"
  where
    Right src = P.parse "+[.-]"
    intape = [1,3,4,5]
    ids = E.run src intape 10 10
    sat = S.gen src intape
    r = D.eval  sat intape ids
    rnot = C.removeNot sat
    cnf = C.toCNF rnot
    (isat, dict) = C.alias cnf
