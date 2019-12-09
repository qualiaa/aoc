import IntCode

main = do
  program <- read . ('[':) . (++"]") <$> getContents
  print $ runProgram program [1]
