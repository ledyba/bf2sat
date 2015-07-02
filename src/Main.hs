import Bf2Sat.Parser as P
import Bf2Sat.SAT as S
import Bf2Sat.Engine as E

main :: IO ()
main =
  case P.parse "[]" of
    Right src -> print $ show $ E.run src [1,3,4,5] 10 10
    -- $ show $ S.genSat src [1,3,4,5]
    Left err -> print $ show err
