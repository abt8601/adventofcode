{-# LANGUAGE TupleSections #-}

import Data.Array (Array, accumArray, bounds, (!), (//))
import Data.Foldable (find)
import Data.Ix (Ix (inRange))
import Data.List.Split (splitOn)

main :: IO ()
main = do
  paths <- map parsePath . lines <$> getContents
  let initCaveMap = synthCaveMap paths

  print $ solve initCaveMap

parsePath :: String -> [(Int, Int)]
parsePath = map parsePoint . splitOn " -> "

parsePoint :: String -> (Int, Int)
parsePoint point = (read x, read y) where (x, _ : y) = break (== ',') point

type CaveMap = Array (Int, Int) Char

synthCaveMap :: [[(Int, Int)]] -> CaveMap
synthCaveMap paths =
  accumArray (\_ x -> x) '.' ((minX, 0), (maxX, maxY)) updates
  where
    points = pathsToPoints paths

    minX = minimum (map fst points)
    maxX = maximum (map fst points)
    maxY = maximum (map snd points)

    updates = map (,'#') points

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

solve :: CaveMap -> Int
solve = go 0
  where
    go acc caveMap = case simSand caveMap of
      Just p -> (go $! acc + 1) $ caveMap // [(p, 'o')]
      Nothing -> acc

initSandPosn :: (Int, Int)
initSandPosn = (500, 0)

simSand :: CaveMap -> Maybe (Int, Int)
simSand = flip simSandAt initSandPosn

simSandAt :: CaveMap -> (Int, Int) -> Maybe (Int, Int)
simSandAt caveMap = go
  where
    go p@(x, y)
      | inCaveMap p =
          let ps = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
           in case find (\p' -> not (inCaveMap p') || caveMap ! p' == '.') ps of
                Just p' -> go p'
                Nothing -> Just p
      | otherwise = Nothing
    inCaveMap = inRange (bounds caveMap)
