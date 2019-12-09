import Data.List (permutations, foldl')
import IntCode

amplifierArray :: ProgramSource -> [Int] -> Int
amplifierArray program phases = foldl' amp 0 phases
  where amp input phase = head $ evalProgram program [phase, input]

main = do
  program <- read . ('[':) . (++"]") <$> getContents
  print $ maximum . map (amplifierArray program) $ permutations [0..4]

