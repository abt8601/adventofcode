main :: IO ()
main = do
  input <- getLine
  print $ findMarker input

findMarker :: String -> Int
findMarker = go 4
  where
    go i (x : xs@(y : z : w : _))
      | isPairwiseDifferent [x, y, z, w] = i
      | otherwise = (go $! i + 1) xs

isPairwiseDifferent :: (Eq a) => [a] -> Bool
isPairwiseDifferent [] = True
isPairwiseDifferent (x : xs) = x `notElem` xs && isPairwiseDifferent xs
