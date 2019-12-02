import Control.Monad ((=<<))

fuel :: Int -> Int
fuel x = x `div` 3 - 2


main = print . sum  . map (fuel . read) . lines =<< getContents
