import Bf2Sat.Parser as P
import Bf2Sat.SAT as S
import Bf2Sat.Engine as E
import Bf2Sat.Debug as D
import Bf2Sat.CNF as C

main :: IO ()
main =
  case P.parse "+[.-]" of
    Right src -> do
      print $ show sat
      print $ show ids
      print $ "Length of IDs: " ++ show (length ids)
      print $ show r
      print $ show src
      writeFile "sat.txt" (C.toDMACS isat dict)
      writeFile "pred.txt" (show dict)
      where
        intape = [1,3,4,5]
        ids = E.run src intape 10 10
        sat = S.gen src intape
        r = D.eval  sat intape ids
        (isat, dict) = C.alias $ C.toCNF $ C.removeNot sat
    -- $ show $ S.genSat src [1,3,4,5]
    Left err -> print $ show err
