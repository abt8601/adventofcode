{-# LANGUAGE TupleSections #-}

import Data.Function (on)
import Data.List (foldl1', groupBy, sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import Data.Vector qualified as V

main :: IO ()
main = do
  (monkeys, initItems) <- parseInput . lines <$> getContents

  let (_, inspectCounts) = simRounds 10000 monkeys initItems
      monkeyBusiness = x * y where (x : y : _) = sortOn Down inspectCounts

  print monkeyBusiness

newtype Monkey = Monkey {throwItem :: Int -> (Int, Int)}

type ItemState = V.Vector [Int]

parseInput :: [String] -> ([Monkey], ItemState)
parseInput input = (monkeys, V.fromList items)
  where
    parseResults = map parseMonkey . splitOn [""] $ input
    modulus = foldl1' lcm . map (\(_, _, _, divisor) -> divisor) $ parseResults
    (monkeys, items) = unzip . map (assembleMonkey modulus) $ parseResults

parseMonkey :: [String] -> ([Int], Int -> Int, Int -> Int, Int)
parseMonkey [_, si, o, t, tt, tf] =
  (startingItems, operation, getTargetMonkey, divisor)
  where
    startingItems = map read . splitOn ", " . drop (length siPrefix) $ si

    operation = case rhsStr of
      "old" -> (\w -> w `op` w)
      _ -> let rhs = read rhsStr in (`op` rhs)
      where
        [opStr, rhsStr] = words (drop (length oPrefix) o)
        op = case opStr of
          "+" -> (+)
          "*" -> (*)
    divisor = read (drop (length tPrefix) t)
    getTargetMonkey w'
      | w' `mod` divisor == 0 = targetIfTrue
      | otherwise = targetIfFalse
      where
        targetIfTrue = read (drop (length ttPrefix) tt)
        targetIfFalse = read (drop (length tfPrefix) tf)

    siPrefix = "  Starting items: "
    oPrefix = "  Operation: new = old "
    tPrefix = "  Test: divisible by "
    ttPrefix = "    If true: throw to monkey "
    tfPrefix = "    If false: throw to monkey "

assembleMonkey :: Int -> ([Int], Int -> Int, Int -> Int, Int) -> (Monkey, [Int])
assembleMonkey modulus (startingItems, operation, getTargetMonkey, _) =
  (Monkey {throwItem = throwItemFn}, startingItems)
  where
    throwItemFn w = (getTargetMonkey w', w')
      where
        w' = operation w `mod` modulus

simRounds :: Int -> [Monkey] -> ItemState -> (ItemState, [Int])
simRounds 0 monkeys st = (st, replicate (length monkeys) 0)
simRounds n monkeys st = (st'', zipWith (+) count count')
  where
    (st', count) = simRound monkeys st
    (st'', count') = simRounds (n - 1) monkeys st'

simRound :: [Monkey] -> ItemState -> (ItemState, [Int])
simRound = foldr f (,[]) . zip [0 ..]
  where
    f (i, monkey) acc st = (st'', count : count')
      where
        (st', count) = simTurn i monkey st
        (st'', count') = acc st'

simTurn :: Int -> Monkey -> ItemState -> (ItemState, Int)
simTurn i Monkey {throwItem = throwItem} st =
  (st V.// updates, length (st V.! i))
  where
    thrownItems = map throwItem (st V.! i)
    groupedThrownItems =
      map (\items@((j, _) : _) -> (j, map snd items))
        . groupBy ((==) `on` fst)
        . sortOn fst
        $ thrownItems
    updates =
      (i, []) : map (\(j, items) -> (j, st V.! j ++ items)) groupedThrownItems
