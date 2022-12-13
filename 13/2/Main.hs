import Data.Char (isDigit)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
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
  packets <- map parsePacket . filter (not . null) . lines <$> getContents

  let sortedPackets = sort (dividerPackets ++ packets)
      dividerIndices =
        map ((+ 1) . fromJust . flip elemIndex sortedPackets) dividerPackets

  print $ product dividerIndices

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

dividerPackets :: [Packet]
dividerPackets = map parsePacket ["[[2]]", "[[6]]"]
