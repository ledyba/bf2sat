import Bf2Sat.Parser as P
import Bf2Sat.SAT as S
import Bf2Sat.Engine as E
import Bf2Sat.Debug as D
import Bf2Sat.CNF as C
import Bf2Sat.RCNF as R

import System.Environment

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
      print pairs
      print "Do not match: "
      print $ filter (\(_,a,v) -> a /= v) pairs
    ("test":_) -> do
      print $ show src
      print $ show ids
      print $ show r
      print $ show rnot
      print $ show cnf
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
