import IntCode

modifyProgram (x:_:_:xs) = x:12:2:xs
main = do
    program <- modifyProgram . read . ('[':) . (++"]") <$> getContents
    print . head $ execProgram program []
