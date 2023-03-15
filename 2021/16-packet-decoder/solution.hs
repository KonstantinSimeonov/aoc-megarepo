import Prelude
import Data.List
import Data.Char (ord)

toBits x = case x of
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'A' -> "1010"
  'B' -> "1011"
  'C' -> "1100"
  'D' -> "1101"
  'E' -> "1110"
  'F' -> "1111"

data Packet =
  Literal Int Int Int
  | Operator Int Int [Packet]
  deriving Show

int = foldl' (\s x -> s * 2 + (ord x - 48)) 0

parsePacket :: String -> (Packet, String)
parsePacket xs =
  case typeId of
    "100" -> parseLiteral xs
    _     -> parseOp xs
  where
    typeId = take 3 $ drop 3 xs

parseOp (a:b:c:d:e:f:l:bits) = (operator, rest)
  where 
    operator = Operator (int [a, b, c]) (int [d, e, f]) packets
    (packets, rest) = case l of
      '0' -> parsePacketsWithLength bits
      '1' -> parsePacketsWithCount bits

parsePacketsWithCount input = parseSubpacket (int countBits) bits
  where
    (countBits, bits) = splitAt 11 input

    parseSubpacket 0 rest = ([], rest)
    parseSubpacket n rest =
      let (packet, rest') = parsePacket rest
          (packets, rest'') = parseSubpacket (n - 1) rest'
      in (packet:packets, rest'')

parsePacketsWithLength input = (packets, rest)
  where
    packets = parseSubpackets subpacketBits
    (subpacketBits, rest) = splitAt (int lenBits) bits
    (lenBits, bits) = splitAt 15 input

    parseSubpackets bits = case parsePacket bits of
      (packet, "") -> [packet]
      (packet, rest) -> packet:parseSubpackets rest

parseLiteral (a:b:c:d:e:f:bits) = (literal, rest)
  where
    literal = Literal (int [a, b, c]) (int [d, e, f]) (int value)
    (value, rest) = parseValue bits

    parseValue ('0':bits) = splitAt 4 bits
    parseValue ('1':a:b:c:d:bits') =
      let (valueBits, rest') = parseValue bits'
      in (a:b:c:d:valueBits, rest')

sumv p = case p of
  Literal v _ _ -> v
  Operator v _ children -> foldl' (\xs c -> xs + sumv c) v children

eval (Literal _ _ v) = v
eval (Operator _ t children)
  | t < 4     =
    let op = [(+), (*), min, max] !! t
    in foldl1' op branches
  | otherwise =
    let [left, right] = branches
        op = [(>), (<), (==)] !! (t - 5)
    in if left `op` right then 1 else 0
  where
    branches = map eval children

render (Literal _ _ v) = show v
render (Operator _ t children) =
  case t of
    2 -> "min" ++ bracketed
    3 -> "max" ++ bracketed
    _ -> bracketed
  where
    bracketed = "(" ++ operands ++ ")"
    operands = intercalate (binary !! t) $ map render children
    binary = [" + ", " - ", ", ", ", ", "", " > ", " < ", " == "]

tests =
  [ "D2FE28"
  , "38006F45291200"
  , "EE00D40C823060"
  , "8A004A801A8002F478"
  , "620080001611562C8802118E34"
  , "C0015000016115A2E0802F182340"
  , "A0016C880162017C3686B18A3D4780"
  , "C200B40A82"
  , "04005AC33890"
  , "880086C3E88112"
  , "CE00C43D881120"
  , "D8005AC2A8F0"
  , "F600BC2D8F"
  , "9C005AC2F8F0"
  , "9C0141080250320F1802104A08"
  , "420D4900B8F31EFE7BD9DA455401AB80021504A2745E1007A21C1C862801F54AD0765BE833D8B9F4CE8564B9BE6C5CC011E00D5C001098F11A232080391521E4799FC5BB3EE1A8C010A00AE256F4963B33391DEE57DA748F5DCC011D00461A4FDC823C900659387DA00A49F5226A54EC378615002A47B364921C201236803349B856119B34C76BD8FB50B6C266EACE400424883880513B62687F38A13BCBEF127782A600B7002A923D4F959A0C94F740A969D0B4C016D00540010B8B70E226080331961C411950F3004F001579BA884DD45A59B40005D8362011C7198C4D0A4B8F73F3348AE40183CC7C86C017997F9BC6A35C220001BD367D08080287914B984D9A46932699675006A702E4E3BCF9EA5EE32600ACBEADC1CD00466446644A6FBC82F9002B734331D261F08020192459B24937D9664200B427963801A094A41CE529075200D5F4013988529EF82CEFED3699F469C8717E6675466007FE67BE815C9E84E2F300257224B256139A9E73637700B6334C63719E71D689B5F91F7BFF9F6EE33D5D72BE210013BCC01882111E31980391423FC4920042E39C7282E4028480021111E1BC6310066374638B200085C2C8DB05540119D229323700924BE0F3F1B527D89E4DB14AD253BFC30C01391F815002A539BA9C4BADB80152692A012CDCF20F35FDF635A9CCC71F261A080356B00565674FBE4ACE9F7C95EC19080371A009025B59BE05E5B59BE04E69322310020724FD3832401D14B4A34D1FE80233578CD224B9181F4C729E97508C017E005F2569D1D92D894BFE76FAC4C5FDDBA990097B2FBF704B40111006A1FC43898200E419859079C00C7003900B8D1002100A49700340090A40216CC00F1002900688201775400A3002C8040B50035802CC60087CC00E1002A4F35815900903285B401AA880391E61144C0004363445583A200CC2C939D3D1A41C66EC40"
  ]

main = do
  sequence_ $ do
      t <- tests
      let bits = concatMap toBits t
      let (tree, _) = parsePacket bits
      pure $ do
          putStrLn $ "expr: " ++ render tree
          putStrLn $ "sumv: " ++ show (sumv tree)
          putStrLn $ "eval: " ++ show (eval tree) ++ "\n"
