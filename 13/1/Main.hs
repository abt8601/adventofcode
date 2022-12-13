import Data.Char (isDigit)
import Data.List (findIndices)
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP
  ( ReadP,
    between,
    char,
    munch1,
    readP_to_S,
    sepBy,
    (<++),
  )

main :: IO ()
main = do
  packetPairs <-
    map (\[p1, p2] -> (parsePacket p1, parsePacket p2)) . splitOn [""] . lines
      <$> getContents

  let packetIndices = map (+ 1) $ findIndices (uncurry (<)) packetPairs

  print $ sum packetIndices

data Packet = Atom Int | List [Packet] deriving (Show, Read)

packet :: ReadP Packet
packet =
  (Atom . read <$> munch1 isDigit)
    <++ (List <$> between (char '[') (char ']') (sepBy packet (char ',')))

parsePacket :: String -> Packet
parsePacket input = result where [(result, "")] = readP_to_S packet input

instance Eq Packet where
  (==) :: Packet -> Packet -> Bool
  p1 == p2 = p1 `compare` p2 == EQ

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  Atom n1 `compare` Atom n2 = n1 `compare` n2
  Atom n1 `compare` List l2 = [Atom n1] `compare` l2
  List l1 `compare` Atom n2 = l1 `compare` [Atom n2]
  List l1 `compare` List l2 = l1 `compare` l2
