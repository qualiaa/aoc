import IntCode

modifyProgram (x:_:_:xs) = x:12:2:xs
main = print =<< head . runProgram . modifyProgram . read . ('[':) . (++"]") <$> getContents
