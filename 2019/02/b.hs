import IntCode
import Control.Arrow (first)

findAnswer :: Int -> Program -> [(Int,Int)]
findAnswer needle (x:_:_:xs) =
  let r = [0..99] in
    [(i,j) | i <- r, j <- r, head (runProgram (x:i:j:xs))]

result = head . map (uncurry (+) . first (*100))
main = print =<< result . findAnswer 19690720 . read . ('[':) . (++"]") <$> getContents
