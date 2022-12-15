import Data.Array (Array, listArray, (!), (//))
import Data.Foldable (Foldable (toList))
import Data.List (transpose)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- lines <$> getContents
  let (crates, procedures) =
        let [crates, procedures] = splitOn [""] input
         in (parseCrates crates, parseProcedures procedures)

  let result = solve crates procedures

  putStrLn result

type Crates = Array Int String

type Procedure = (Int, Int, Int)

type Procedures = [Procedure]

parseCrates :: [String] -> Crates
parseCrates crates =
  let parseResults = map parseRow crates
      header = last parseResults
      nCrates = length header
   in listArray (1, nCrates) . map (dropWhile (== ' ')) . transpose . init $
        parseResults
  where
    parseRow [_, crate, _] = [crate]
    parseRow (_ : crate : _ : _ : rest) = crate : parseRow rest

parseProcedures :: [String] -> Procedures
parseProcedures = map parseProcedure
  where
    parseProcedure s =
      let [_, amt, _, from, _, to] = words s
       in (read amt, read from, read to)

solve :: Crates -> Procedures -> String
solve crates procedures = map head . toList $ applyProcedures crates procedures

applyProcedures :: Crates -> Procedures -> Crates
applyProcedures = foldl applyProcedure

applyProcedure :: Crates -> Procedure -> Crates
applyProcedure crates (amt, from, to) =
  let (moved, rest) = splitAt amt $ crates ! from
   in crates // [(from, rest), (to, moved ++ crates ! to)]
