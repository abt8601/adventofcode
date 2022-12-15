import Data.Char (isLower, isUpper, ord)
import Data.List.Split (chunksOf)
import Data.Set qualified as S

main :: IO ()
main = do
  input <- lines <$> getContents
  let result = solve input
  print result

solve :: [String] -> Int
solve = sum . map priority . findBadges

findBadges :: [String] -> [Char]
findBadges = map findBadgeOfGroup . chunksOf 3

findBadgeOfGroup :: [String] -> Char
findBadgeOfGroup [r0, r1, r2] =
  let [badge] =
        S.toList $
          S.intersection
            (S.intersection (S.fromList r0) (S.fromList r1))
            (S.fromList r2)
   in badge

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | isUpper c = ord c - ord 'A' + 27
