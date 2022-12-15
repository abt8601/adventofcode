{-# LANGUAGE TupleSections #-}

import Data.Array (Array, assocs, bounds, listArray, (!), (//))
import Data.Char (ord)
import Data.Foldable (Foldable (..))
import Data.Ix (inRange)
import Data.List (find)
import Data.Semigroup (Arg (..))

main :: IO ()
main = do
  heightmap <- do
    heightmap <- lines <$> getContents
    let m = length heightmap
        n = length (head heightmap)
    return $ listArray ((0, 0), (m - 1, n - 1)) (concat heightmap)

  print $ solve heightmap

type Heightmap = Array (Int, Int) Char

solve :: Heightmap -> Int
solve heightmap = astar initFrontier initCosts
  where
    (_, (mm1, nm1)) = bounds heightmap
    m = mm1 + 1
    n = nm1 + 1

    startPosns = map fst . filter ((`elem` "Sa") . snd) $ assocs heightmap
    Just (endPosn@(ei, ej), _) = find ((== 'E') . snd) (assocs heightmap)

    heuristic (i, j) = abs (i - ei) + abs (j - ej)

    heightOf p = case heightmap ! p of
      'S' -> 0
      'E' -> 25
      h -> ord h - ord 'a'

    neighbours p@(i, j) =
      [ q
        | (di, dj) <- [(1, 0), (0, 1), (-1, 0), (0, -1)],
          let q = (i + di, j + dj),
          inRange (bounds heightmap) q,
          heightOf q <= heightOf p + 1
      ]

    initFrontier = foldl' f Empty startPosns
      where
        f frontier p = insert (Arg (heuristic p) p) frontier
    initCosts =
      listArray ((0, 0), (mm1, nm1)) (replicate (m * n) maxBound)
        // map (,0) startPosns

    astar frontier costs = case extractMin frontier of
      Nothing -> error "astar: no path"
      Just (Arg priority current, frontier')
        | current == endPosn -> priority
        | otherwise ->
            let (frontier'', costs') =
                  foldl' f (frontier', costs) (neighbours current)
             in astar frontier'' costs'
        where
          f (frontier, costs) neighbour
            | costs ! neighbour > newCost =
                let frontierEntry =
                      Arg (newCost + heuristic neighbour) neighbour
                    frontier' = insert frontierEntry frontier
                    costs' = costs // [(neighbour, newCost)]
                 in frontier' `seq` costs' `seq` (frontier', costs')
            | otherwise = (frontier, costs)
            where
              currentCost = costs ! current
              newCost = currentCost + 1

data SkewHeap a
  = Empty
  | Node a (SkewHeap a) (SkewHeap a)

singleton :: Ord a => a -> SkewHeap a
singleton x = Node x Empty Empty

union :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
Empty `union` t2 = t2
t1 `union` Empty = t1
t1@(Node x1 l1 r1) `union` t2@(Node x2 l2 r2)
  | x1 <= x2 = Node x1 (t2 `union` r1) l1
  | otherwise = Node x2 (t1 `union` r2) l2

insert :: Ord a => a -> SkewHeap a -> SkewHeap a
insert x heap = singleton x `union` heap

extractMin :: Ord a => SkewHeap a -> Maybe (a, SkewHeap a)
extractMin Empty = Nothing
extractMin (Node x l r) = Just (x, l `union` r)
