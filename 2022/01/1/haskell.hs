import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- getContents
  print $ maximum . map (sum . map read) . splitOn [""] . lines $ input
