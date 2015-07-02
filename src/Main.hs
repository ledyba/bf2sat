import Bf2Sat.Parser as P
import Bf2Sat.SAT as S

main :: IO ()
main =
  case P.parse "[]" of
    Right src -> print $ show $ S.genSat src [1,3,4,5]
    Left err -> print $ show err
