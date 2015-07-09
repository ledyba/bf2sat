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
    ("run":_) -> do
      print $ show sat
      print $ show ids
      print $ "Length of IDs: " ++ show (length ids)
      print $ show r
      print $ show src
      writeFile "sat.txt" (C.toDMACS isat dict)
      writeFile "pred.txt" (show dict)
    _ -> do
      preds <- fmap (read :: String -> [(Int, Component)]) (readFile "pred.txt")
      ans <- readFile "ans.txt"
      print $ show $ R.fromDMACS preds ans
  where
    Right src = P.parse "+[.-]"
    intape = [1,3,4,5]
    ids = E.run src intape 10 10
    sat = S.gen src intape
    r = D.eval  sat intape ids
    (isat, dict) = C.alias $ C.toCNF $ C.removeNot sat
