-- Based on row scanning. Uses parts of the solution to part 1 as building
-- blocks.
import Data.Monoid (First (..))
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
solve sensors = x * 4000000 + y
  where
    Just (x, y) = findDistressBeaconPosn sensors

findDistressBeaconPosn :: [(Point, Point)] -> Maybe Point
findDistressBeaconPosn sensors = getFirst $ foldMap (First . f) [minY .. maxY]
  where
    f y =
      (,y)
        <$> firstExcludedPointBetween
          (beaconExclusionZoneAt sensors y)
          (minX, maxX)

beaconExclusionZoneAt :: [(Point, Point)] -> Int -> Intervals
beaconExclusionZoneAt sensors targetY = foldr f empty sensors
  where
    f ((sx, sy), (bx, by)) acc
      | dx >= 0 = addInterval (sx - dx, sx + dx) acc
      | otherwise = acc
      where
        d = abs (sx - bx) + abs (sy - by)
        dx = d - abs (sy - targetY)

minX :: Int
minX = 0

maxX :: Int
maxX = 4000000

minY :: Int
minY = 0

maxY :: Int
maxY = 4000000

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

firstExcludedPointBetween :: Intervals -> (Int, Int) -> Maybe Int
firstExcludedPointBetween (Intervals []) _ = Nothing
firstExcludedPointBetween (Intervals ((_, b) : intervals)) bounds@(from, to)
  | candidate > to = Nothing
  | candidate >= from = Just candidate
  | otherwise = firstExcludedPointBetween (Intervals intervals) bounds
  where
    candidate = b + 1
