import Data.HashSet qualified as S

main :: IO ()
main = do
  motions <-
    map ((\[[direction], nSteps] -> (direction, read nSteps)) . words) . lines
      <$> getContents

  print $ solve motions

type Point = (Int, Int)

type Rope = (Point, Point)

type Motion = (Char, Int)

solve :: [Motion] -> Int
solve motions = S.size $ S.fromList (map snd (lastRope : trace))
  where
    (trace, lastRope) = traceMotions motions ((0, 0), (0, 0))

traceMotions :: [Motion] -> Rope -> ([Rope], Rope)
traceMotions [] rope = ([], rope)
traceMotions (motion : motions) rope = (trace ++ trace', rope'')
  where
    (trace, rope') = traceMotion motion rope
    (trace', rope'') = traceMotions motions rope'

traceMotion :: Motion -> Rope -> ([Rope], Rope)
traceMotion (_, 0) rope = ([], rope)
traceMotion (direction, nSteps) rope = (rope : trace, rope')
  where
    (trace, rope') =
      traceMotion (direction, nSteps - 1) $ applySingleMotion direction rope

applySingleMotion :: Char -> Rope -> Rope
applySingleMotion direction ((hi, hj), (ti, tj)) = (headPosn', tailPosn')
  where
    headPosn' = case direction of
      'U' -> (hi - 1, hj)
      'D' -> (hi + 1, hj)
      'L' -> (hi, hj - 1)
      'R' -> (hi, hj + 1)
    tailPosn'
      | hi > ti + 1 = (ti + 1, tj + signum (hj - tj))
      | hi < ti - 1 = (ti - 1, tj + signum (hj - tj))
      | hj > tj + 1 = (ti + signum (hi - ti), tj + 1)
      | hj < tj - 1 = (ti + signum (hi - ti), tj - 1)
      | otherwise = (ti, tj)
