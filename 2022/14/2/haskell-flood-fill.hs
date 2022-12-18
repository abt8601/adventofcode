-- Based on flood fill algorithm. Runs much faster than the simulation-based
-- solution.
{-# LANGUAGE TupleSections #-}

import Data.HashSet (HashSet)
import Data.HashSet qualified as S
import Data.List.Split (splitOn)

main :: IO ()
main = do
  paths <- map parsePath . lines <$> getContents

  print $ solve paths

parsePath :: String -> [(Int, Int)]
parsePath = map parsePoint . splitOn " -> "

parsePoint :: String -> (Int, Int)
parsePoint point = (read x, read y) where (x, _ : y) = break (== ',') point

solve :: [[(Int, Int)]] -> Int
solve paths = S.size $ floodFill rocks maxY initSandPosn S.empty
  where
    rocks = S.fromList $ pathsToPoints paths
    maxY = maximum (map snd (concat paths)) + 1

pathsToPoints :: [[(Int, Int)]] -> [(Int, Int)]
pathsToPoints = concatMap pathToPoints

pathToPoints :: [(Int, Int)] -> [(Int, Int)]
pathToPoints [] = []
pathToPoints [p] = [p]
pathToPoints ((x1, y1) : ps@((x2, y2) : _)) = points ++ pathToPoints ps
  where
    points
      | x1 == x2 =
          let ys = case y1 `compare` y2 of
                LT -> [y1 .. y2 - 1]
                EQ -> []
                GT -> [y1, y1 - 1 .. y2 + 1]
           in map (x1,) ys
      | y1 == y2 =
          let xs = case x1 `compare` x2 of
                LT -> [x1 .. x2 - 1]
                EQ -> []
                GT -> [x1, x1 - 1 .. x2 + 1]
           in map (,y1) xs

initSandPosn :: (Int, Int)
initSandPosn = (500, 0)

floodFill ::
  HashSet (Int, Int) ->
  Int ->
  (Int, Int) ->
  HashSet (Int, Int) ->
  HashSet (Int, Int)
floodFill rocks maxY = go
  where
    go p@(x, y) visited
      | y > maxY || p `S.member` rocks || p `S.member` visited = visited
      | otherwise =
          foldr go (S.insert p visited) [(x', y + 1) | x' <- [x - 1 .. x + 1]]
