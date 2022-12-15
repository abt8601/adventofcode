main :: IO ()
main = do
  input <- map parseRanges . lines <$> getContents
  let result = solve input
  print result

type Range = (Integer, Integer)

parseRanges :: String -> (Range, Range)
parseRanges s =
  let (r1, _ : r2) = break (== ',') s in (parseRange r1, parseRange r2)

parseRange :: String -> Range
parseRange str = let (s, _ : t) = break (== '-') str in (read s, read t)

solve :: [(Range, Range)] -> Int
solve = length . filter (uncurry hasOverlap)

hasOverlap :: Range -> Range -> Bool
hasOverlap (s1, t1) (s2, t2) = max s1 s2 <= min t1 t2
