import Data.Array (Array, bounds, listArray, (!))
import Data.Char (digitToInt)
import Data.HashSet qualified as S

main :: IO ()
main = do
  treeMap <- do
    treeMap <- map (map digitToInt) . lines <$> getContents
    let m = length treeMap
        n = length (head treeMap)
    return $ listArray ((0, 0), (m - 1, n - 1)) (concat treeMap)

  print $ solve treeMap

type TreeMap = Array (Int, Int) Int

solve :: TreeMap -> Int
solve = S.size . visibleIxs

visibleIxs :: TreeMap -> S.HashSet (Int, Int)
visibleIxs treeMap = S.fromList $ concatMap (visibleIxsAlong treeMap) rays
  where
    (_, (mm1, nm1)) = bounds treeMap

    rays = raysUp ++ raysDown ++ raysLeft ++ raysRight
    raysUp = [[(i, j) | i <- [mm1, mm1 - 1 .. 0]] | j <- [0 .. nm1]]
    raysDown = [[(i, j) | i <- [0 .. mm1]] | j <- [0 .. nm1]]
    raysLeft = [[(i, j) | j <- [nm1, nm1 - 1 .. 0]] | i <- [0 .. mm1]]
    raysRight = [[(i, j) | j <- [0 .. nm1]] | i <- [0 .. mm1]]

visibleIxsAlong :: TreeMap -> [(Int, Int)] -> [(Int, Int)]
visibleIxsAlong treeMap ixs = foldr f (const []) ixs (-1)
  where
    f ix acc prevHeight
      | height > prevHeight = ix : acc height
      | otherwise = acc prevHeight
      where
        height = treeMap ! ix
