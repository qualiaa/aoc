read' ('+':s) = read s
read' s = read s

main = sum . map read' . lines <$> getContents >>= print
