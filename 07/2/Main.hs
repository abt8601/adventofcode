import Data.Foldable (Foldable (..))
import Data.HashMap.Strict qualified as M
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- lines <$> getContents
  let fs = constructFS (parseCmds input)

  print $ findDirSize fs

parseCmds :: [String] -> [(String, [String])]
parseCmds [] = []
parseCmds (cmd : lns) = (drop 2 cmd, output) : parseCmds rest
  where
    (output, rest) = break ("$ " `isPrefixOf`) lns

type Dir = M.HashMap String Item

data Item = File Int | Directory Dir deriving (Eq, Ord, Show, Read)

constructFS :: [(String, [String])] -> Dir
constructFS cmds = foldr f (\_ fs -> fs) cmds [] M.empty
  where
    f (cmd, output) acc wd fs = case words cmd of
      ["cd", dir] -> acc (cd dir wd) fs
      ["ls"] -> acc wd $! addItemsAt wd output fs

cd :: String -> [String] -> [String]
cd ".." [] = []
cd ".." wd = init wd
cd "/" _ = []
cd dir wd = wd ++ [dir]

addItemsAt :: [String] -> [String] -> Dir -> Dir
addItemsAt [] items fs = addItems items fs
addItemsAt (dir : dirs) items fs =
  M.alter (Just . Directory . addItemsAt dirs items . getOrMk) dir fs
  where
    getOrMk = maybe M.empty (\(Directory dir) -> dir)

addItems :: [String] -> Dir -> Dir
addItems = flip $ foldl' (flip addItem)

addItem :: String -> Dir -> Dir
addItem item = M.alter (Just . fromMaybe dirItem) name
  where
    (name, dirItem) = case words item of
      ["dir", name] -> (name, Directory M.empty)
      [size, name] -> (name, File (read size))

targetSize :: Int
targetSize = 30000000

diskSize :: Int
diskSize = 70000000

findDirSize :: Dir -> Int
findDirSize fs = minimum . filter (>= requiredSize) $ sizes
  where
    sizes@(rootSize : _) = allSubdirSizes fs
    currentFree = diskSize - rootSize
    requiredSize = targetSize - currentFree

allSubdirSizes :: Dir -> [Int]
allSubdirSizes fs = sumDirSize + sumFileSize : concat subdirSizes
  where
    subdirs = [dir | (_, Directory dir) <- M.toList fs]
    subdirSizes = map allSubdirSizes subdirs

    sumDirSize = sum (map head subdirSizes)
    sumFileSize = sum [size | (_, File size) <- M.toList fs]
