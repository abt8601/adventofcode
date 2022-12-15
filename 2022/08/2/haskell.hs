import Data.Array (Array, bounds, indices, listArray, (!))
import Data.Char (digitToInt)

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
solve treeMap = maximum . map (scenicScore treeMap) $ indices treeMap

scenicScore :: TreeMap -> (Int, Int) -> Int
scenicScore treeMap ix@(i, j) = product (map (nTreesVisibleAlong treeMap ix) rays)
  where
    (_, (mm1, nm1)) = bounds treeMap

    rays = [rayUp, rayDown, rayLeft, rayRight]
    rayUp = [(i', j) | i' <- [i - 1, i - 2 .. 0]]
    rayDown = [(i', j) | i' <- [i + 1 .. mm1]]
    rayLeft = [(i, j') | j' <- [j - 1, j - 2 .. 0]]
    rayRight = [(i, j') | j' <- [j + 1 .. nm1]]

nTreesVisibleAlong :: TreeMap -> (Int, Int) -> [(Int, Int)] -> Int
nTreesVisibleAlong treeMap initIx ixs = foldr f id ixs 0
  where
    f ix acc count
      | treeMap ! ix < maxHeight = acc $! count + 1
      | otherwise = count + 1
    maxHeight = treeMap ! initIx
