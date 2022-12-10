import Data.List.Split (chunksOf)

main :: IO ()
main = do
  insns <- lines <$> getContents
  putStr $ unlines (render insns)

render :: [String] -> [String]
render insns = take 6 . map (zipWith f [0 ..]) . chunksOf 40 $ trace insns
  where
    f i x
      | abs (i - x) <= 1 = '#'
      | otherwise = '.'

trace :: [String] -> [Int]
trace insns = foldr f (const []) insns 1
  where
    f insn acc reg = case words insn of
      ["addx", v] -> reg : reg : acc (reg + read v)
      ["noop"] -> reg : acc reg
