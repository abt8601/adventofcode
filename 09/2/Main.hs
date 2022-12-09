import Data.HashSet qualified as S

main :: IO ()
main = do
  motions <-
    map ((\[[direction], nSteps] -> (direction, read nSteps)) . words) . lines
      <$> getContents

  print $ solve motions

type Point = (Int, Int)

type Rope = [Point]

type Motion = (Char, Int)

solve :: [Motion] -> Int
solve motions = S.size $ S.fromList (map last (lastRope : trace))
  where
    (trace, lastRope) = traceMotions motions (replicate 10 (0, 0))

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
applySingleMotion direction ((hi, hj) : knots) =
  headPosn' : updateKnots headPosn' knots
  where
    headPosn' = case direction of
      'U' -> (hi - 1, hj)
      'D' -> (hi + 1, hj)
      'L' -> (hi, hj - 1)
      'R' -> (hi, hj + 1)
    updateKnots _ [] = []
    updateKnots (pi, pj) ((ki, kj) : knots) =
      knotPosn' : updateKnots knotPosn' knots
      where
        knotPosn'
          | pi > ki + 1 = (ki + 1, kj + signum (pj - kj))
          | pi < ki - 1 = (ki - 1, kj + signum (pj - kj))
          | pj > kj + 1 = (ki + signum (pi - ki), kj + 1)
          | pj < kj - 1 = (ki + signum (pi - ki), kj - 1)
          | otherwise = (ki, kj)
