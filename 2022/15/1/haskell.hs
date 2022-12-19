import Text.Regex.Posix ((=~))

main :: IO ()
main = do
  sensors <- map parseSensor . lines <$> getContents

  print $ solve sensors

type Point = (Int, Int)

parseSensor :: String -> (Point, Point)
parseSensor input = ((read sx, read sy), (read bx, read by))
  where
    (_, _, _, [sx, sy, bx, by]) =
      input =~ inputPat :: (String, String, String, [String])

inputPat :: String
inputPat = "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)"

solve :: [(Point, Point)] -> Int
solve = nPoints . beaconExclusionZone

beaconExclusionZone :: [(Point, Point)] -> Intervals
beaconExclusionZone = foldr f empty
  where
    f ((sx, sy), (bx, by)) acc
      | dx >= 0 =
          let a = sx - dx
              b = sx + dx
           in case (by == targetY, bx == a, bx == b) of
                (True, True, True) -> acc
                (True, True, False) -> addInterval (a + 1, b) acc
                (True, False, True) -> addInterval (a, b - 1) acc
                (False, _, _) -> addInterval (a, b) acc
      | otherwise = acc
      where
        d = abs (sx - bx) + abs (sy - by)
        dx = d - abs (sy - targetY)

targetY :: Int
targetY = 2000000

type Interval = (Int, Int)

newtype Intervals = Intervals [Interval] deriving (Eq, Ord, Show, Read)

empty :: Intervals
empty = Intervals []

addInterval :: Interval -> Intervals -> Intervals
addInterval newInterval (Intervals []) = Intervals [newInterval]
addInterval newInterval@(na, nb) (Intervals (interval@(a, b) : intervals))
  | nb + 1 < a = Intervals (newInterval : interval : intervals)
  | b + 1 < na =
      let Intervals result = addInterval newInterval (Intervals intervals)
       in Intervals (interval : result)
  | otherwise = addInterval (min na a, max nb b) (Intervals intervals)

nPoints :: Intervals -> Int
nPoints (Intervals intervals) = sum (map (\(a, b) -> b - a + 1) intervals)
