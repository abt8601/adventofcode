main :: IO ()
main = do
  insns <- lines <$> getContents
  print $ solve insns

solve :: [String] -> Int
solve insns =
  sum $ map (signalStrengthAt (trace insns)) [20, 60, 100, 140, 180, 220]

signalStrengthAt :: [Int] -> Int -> Int
signalStrengthAt tr i = i * tr !! (i - 1)

trace :: [String] -> [Int]
trace insns = foldr f (const []) insns 1
  where
    f insn acc reg = case words insn of
      ["addx", v] -> reg : reg : acc (reg + read v)
      ["noop"] -> reg : acc reg
