main :: IO ()
main = do
  input <- getLine
  print $ findMarker input

findMarker :: String -> Int
findMarker = go markerLength
  where
    markerLength = 14

    go i xs@(_ : ys)
      | isPairwiseDifferent (take markerLength xs) = i
      | otherwise = (go $! i + 1) ys

isPairwiseDifferent :: (Eq a) => [a] -> Bool
isPairwiseDifferent [] = True
isPairwiseDifferent (x : xs) = x `notElem` xs && isPairwiseDifferent xs
