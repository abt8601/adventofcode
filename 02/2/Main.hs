main :: IO ()
main = do
  input <- map (\line -> (head line, line !! 2)) . lines <$> getContents
  let result = solve input
  print result

solve :: [(Char, Char)] -> Int
solve = sum . map (uncurry score)

score :: Char -> Char -> Int
score opponent outcome =
  shapeScore (chooseShape opponent outcome) + outcomeScore outcome

chooseShape :: Char -> Char -> Char
chooseShape 'A' 'X' = 'Z'
chooseShape 'A' 'Y' = 'X'
chooseShape 'A' 'Z' = 'Y'
chooseShape 'B' 'X' = 'X'
chooseShape 'B' 'Y' = 'Y'
chooseShape 'B' 'Z' = 'Z'
chooseShape 'C' 'X' = 'Y'
chooseShape 'C' 'Y' = 'Z'
chooseShape 'C' 'Z' = 'X'

shapeScore :: Char -> Int
shapeScore 'X' = 1
shapeScore 'Y' = 2
shapeScore 'Z' = 3

outcomeScore :: Char -> Int
outcomeScore 'X' = 0
outcomeScore 'Y' = 3
outcomeScore 'Z' = 6
