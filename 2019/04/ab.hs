import Data.List (any, all, group, and)

digits 0 = []
digits n = let (q,r) = n `divMod` 10 in r:digits q
digitPairs n = let d = digits n in zip d $ tail d

gt  = uncurry (<)
eq  = uncurry (==)

main = do
  (l,_:u) <- break (=='-') <$> getLine
  let candidates = [n | n <- [(read l) .. (read u)], let pairs=digitPairs n,
                        not $ any gt pairs, any eq pairs]

  print $ length candidates
  print $ length [n | n <- candidates, any (==2) . map length . group $ digits n]
