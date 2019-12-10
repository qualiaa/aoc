import IntCode

main = do
  program <- read . ('[':) . (++"]") <$> getContents
  print . last  $ evalProgram program [1]
  print . last  $ evalProgram program [2]
