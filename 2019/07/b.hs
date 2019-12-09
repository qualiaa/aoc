import Data.List (permutations, foldl', scanl')
import IntCode

feedback :: [Coroutine] -> Input -> Output
feedback (c:cs) i = let result = scanl' (\(_,i) c -> resume c i) (resume c i) cs
                    in case last result of
                         ((Finished _ _), o) -> o
                         (_, o) -> feedback (map fst result) o

amplifierArray :: ProgramSource -> [Int] -> [Int]
amplifierArray program phases = feedback (map amp phases) [0]
  where amp phase = fst $ coroutine program [phase]

main = do
  program <- read . ('[':) . (++"]") <$> getContents
  print $ maximum . map (last . amplifierArray program) $ permutations [5..9]
