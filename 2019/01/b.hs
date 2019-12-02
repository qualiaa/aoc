import Control.Monad ((=<<))

fuel x = x `div` 3 - 2
ifuel = sum . takeWhile (>0) . iterate fuel . fuel
main = print . sum  . map (ifuel . read) . lines =<< getContents
