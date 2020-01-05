import IntCode

takeEvery n l = case splitAt (n-1) l of
                  (_, []) -> []
                  (_, x:xs) -> x : takeEvery n xs

main = do
  program <- read . ('[':) . (++"]") <$> getContents
  print . length . filter (==2) . takeEvery 3 $ evalProgram program []
