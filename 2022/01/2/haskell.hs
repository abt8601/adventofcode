import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))

main :: IO ()
main = do
  input <- getContents
  print $
    sum . take 3 . sortOn Down . map (sum . map read) . splitOn [""] . lines $
      input
