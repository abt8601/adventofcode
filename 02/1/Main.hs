main :: IO ()
main = do
  input <- map (\line -> (head line, line !! 2)) . lines <$> getContents
  let result = solve input
  print result

solve :: [(Char, Char)] -> Int
solve = sum . map (uncurry score)

score :: Char -> Char -> Int
score opponent self = shapeScore self + outcomeScore opponent self

shapeScore :: Char -> Int
shapeScore 'X' = 1
shapeScore 'Y' = 2
shapeScore 'Z' = 3

outcomeScore :: Char -> Char -> Int
outcomeScore 'A' 'X' = 3
outcomeScore 'A' 'Y' = 6
outcomeScore 'A' 'Z' = 0
outcomeScore 'B' 'X' = 0
outcomeScore 'B' 'Y' = 3
outcomeScore 'B' 'Z' = 6
outcomeScore 'C' 'X' = 6
outcomeScore 'C' 'Y' = 0
outcomeScore 'C' 'Z' = 3
