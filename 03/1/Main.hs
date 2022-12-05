import Data.Char (isLower, isUpper, ord)
import Data.Set qualified as S

main :: IO ()
main = do
  input <- lines <$> getContents
  let result = solve input
  print result

solve :: [String] -> Int
solve = sum . map solveRucksack

solveRucksack :: String -> Int
solveRucksack rucksack =
  sum . map priority . S.toList $ S.intersection (S.fromList c1) (S.fromList c2)
  where
    (c1, c2) = splitAt (length rucksack `div` 2) rucksack

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27
