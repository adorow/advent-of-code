----- Part Two ---
--Now that you have the structure of your transmission decoded, you can calculate the value of the expression it represents.
--
--Literal values (type ID 4) represent a single number as described above. The remaining type IDs are more interesting:
--
--Packets with type ID 0 are sum packets - their value is the sum of the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
--Packets with type ID 1 are product packets - their value is the result of multiplying together the values of their sub-packets. If they only have a single sub-packet, their value is the value of the sub-packet.
--Packets with type ID 2 are minimum packets - their value is the minimum of the values of their sub-packets.
--Packets with type ID 3 are maximum packets - their value is the maximum of the values of their sub-packets.
--Packets with type ID 5 are greater than packets - their value is 1 if the value of the first sub-packet is greater than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
--Packets with type ID 6 are less than packets - their value is 1 if the value of the first sub-packet is less than the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
--Packets with type ID 7 are equal to packets - their value is 1 if the value of the first sub-packet is equal to the value of the second sub-packet; otherwise, their value is 0. These packets always have exactly two sub-packets.
--Using these rules, you can now work out the value of the outermost packet in your BITS transmission.
--
--For example:
--
--C200B40A82 finds the sum of 1 and 2, resulting in the value 3.
--04005AC33890 finds the product of 6 and 9, resulting in the value 54.
--880086C3E88112 finds the minimum of 7, 8, and 9, resulting in the value 7.
--CE00C43D881120 finds the maximum of 7, 8, and 9, resulting in the value 9.
--D8005AC2A8F0 produces 1, because 5 is less than 15.
--F600BC2D8F produces 0, because 5 is not greater than 15.
--9C005AC2F8F0 produces 0, because 5 is not equal to 15.
--9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.
--What do you get if you evaluate the expression represented by your hexadecimal-encoded BITS transmission?

-- run with: `ghc -o runner ./2021/src/Day16_PacketDecoder_Part1.hs && ./runner`

import System.IO

-- input is in hex, needs to be converted to binary
-- transmission contains a single PACKET in the outer layer (which contains many packets)
-- every packet (or the outer only?) might have a few extra 0s at the end that should be ignored
-- every packet:
--      - first 3 bits are the 'version'
--      - next 3 bits are the 'type ID'
-- on types:
--      - 4 (0b100) - literal value -> it follows 1+ groups of 5 bits, where the first bit says if there's more packets after this one, and the following 4 bits are part of the number
--      - every other value represents an operator
--          - operators contain one of more packets
--          - the bit after the type is the 'length type ID'
-- on length type ID (one bit):
--      - 0 => the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
--      - 1 => the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.

type Version = Int
type HexString = String
type BinString = String
type HexChar = Char
type BinChar = Char

data Expression = Literal Int | Sum [Packet] | Product [Packet] | Minimum [Packet] | Maximum [Packet] | GreaterThan [Packet] | LessThan [Packet] | EqualTo [Packet] deriving (Show)
data Packet = Packet {
    version :: Version,
    expression :: Expression
} deriving (Show)

-- maybe can turn ParsedWithLeftover into a Monad to simplify some things?
type ParsedWithLeftover a = (a, BinString)

class Eval a where
    eval :: a -> Int


instance Eval Packet where
    eval Packet { version = _, expression = expr } = eval expr
--    eval Packet { version = _, expression = expr } = 0

instance Eval Expression where
    eval (Literal n) = n
    eval (Sum xs) = sum $ (map eval xs)
    eval (Product xs) = product $ (map eval xs)
    eval (Minimum xs) = minimum $ (map eval xs)
    eval (Maximum xs) = maximum $ (map eval xs)
    eval (GreaterThan (a:b:[])) = if (eval a) > (eval b) then 1 else 0
    eval (LessThan (a:b:[])) = if (eval a) < (eval b) then 1 else 0
    eval (EqualTo (a:b:[])) = if (eval a) == (eval b) then 1 else 0

main = do
    withFile "./2021/resources/day16.in" ReadMode (\handle -> do
        contents <- hGetContents handle
        let (outerPacket, leftover) = decodePacket $ hexToBin contents
--        let (outerPacket, _) = decodePacket $ hexToBin contents
--        let result = decodePacket $ "110100101111111000101000" -- packet with literal (version 6, value 2021), 3 zero-bits are left over
--        let result = decodePacket $ "00111000000000000110111101000101001010010001001000000000" -- packet with length in bits (version 1, type ID 6, 27 bits of content:> 11 bits with literal 10, 16 bits with literal 20), 7 zero-bits are left over
--        let result = decodePacket $ "11101110000000001101010000001100100000100011000001100000" -- packet with n sub-packets (version 7, type ID 3, 3 sub-packets :> 11 bits with literal 1, 11 bits with literal 2, 11 bits with literal 3), 5 zero-bits are left over
        putStrLn $ show outerPacket
        putStrLn $ "Leftover: " ++ (show leftover)
        putStrLn . show $ eval outerPacket)

decodePacket :: BinString -> ParsedWithLeftover Packet
decodePacket bin = (Packet { version = version, expression = expression}, leftover)
    where version = binToInt (take 3 bin)
          typeId = binToInt (take 3 . drop 3 $ bin)
          remainingBin = (drop 6 bin)
          (expression, leftover) = decodeExpression typeId remainingBin

decodeExpression :: Int -> BinString -> ParsedWithLeftover Expression
-- typeId=4 => literal
decodeExpression 4 bin = decodeLiteral bin
-- any other => operations
decodeExpression typeId bin
    | typeId == 0 = (Sum packets, leftover)
    | typeId == 1 = (Product packets, leftover)
    | typeId == 2 = (Minimum packets, leftover)
    | typeId == 3 = (Maximum packets, leftover)
    | typeId == 5 = (GreaterThan packets, leftover)
    | typeId == 6 = (LessThan packets, leftover)
    | typeId == 7 = (EqualTo packets, leftover)
    where (packets, leftover) = decodeOperationPackets bin
--decodeExpression 0 bin = decodeOperation bin
--decodeExpression 1 bin = decodeOperation bin
--decodeExpression 2 bin = decodeOperation bin
--decodeExpression 3 bin = decodeOperation bin
--decodeExpression 5 bin = decodeOperation bin
--decodeExpression 6 bin = decodeOperation bin
--decodeExpression 7 bin = decodeOperation bin


decodeLiteral :: String -> ParsedWithLeftover Expression
decodeLiteral bin = (Literal (binToInt binLiteral), leftover)
    where (binLiteral, leftover) = readLiteralBin bin


-- this function reads the input binary (sequence of 5 packet digits, where the first is a flag dictating if there are more packets of 5 to read)
readLiteralBin :: BinString -> ParsedWithLeftover BinString
readLiteralBin ('0':xs) = ((take 4 xs), drop 4 xs) -- take the first 4, the rest is leftover)
--readLiteralBin ('1':xs) = (take 4 xs) ++ (readLiteralBin (drop 4 xs))
readLiteralBin ('1':xs) = ((take 4 xs) ++ bin, leftover)
    where (bin, leftover) = readLiteralBin (drop 4 xs)


decodeOperationPackets :: BinString -> ParsedWithLeftover [Packet]
-- type=0 => next 15 bits are the total length in bits
-- type=1 => next 11 bits are the number of sub-packets immediately contained
decodeOperationPackets ('0':xs) = decodePacketsOfBitSize numberOfBitsSubPackets remaining
    where numberOfBitsSubPackets = binToInt (take 15 xs)
          remaining = (drop 15 xs)
decodeOperationPackets ('1':xs) = decodePacketsOfLength numberSubPackets remaining
    where numberSubPackets = binToInt (take 11 xs)
          remaining = (drop 11 xs)


decodePacketsOfBitSize :: Int -> BinString -> ParsedWithLeftover [Packet]
decodePacketsOfBitSize 0 bin = ([], bin)
decodePacketsOfBitSize size bin = (packet:remainingPackets, leftover)
    where (packet, leftoverOfPacket) = decodePacket bin
          bitsRead = (length bin) - (length leftoverOfPacket)
          (remainingPackets, leftover) = decodePacketsOfBitSize (size - bitsRead) leftoverOfPacket

decodePacketsOfLength :: Int -> BinString -> ParsedWithLeftover [Packet]
decodePacketsOfLength 0 bin = ([], bin)
decodePacketsOfLength n bin = (packet:remainingPackets, leftover)
    where (packet, leftoverOfPacket) = decodePacket bin
          (remainingPackets, leftover) = decodePacketsOfLength (n-1) leftoverOfPacket


binToInt :: BinString -> Int
binToInt str = binToInt' 0 str


binToInt' :: Int -> BinString -> Int
binToInt' acc [] = acc
binToInt' acc (x:xs) = binToInt' ((acc * 2) + (binCharToInt x)) xs


binCharToInt :: BinChar -> Int
binCharToInt '0' = 0
binCharToInt '1' = 1


hexToBin :: HexString -> BinString
hexToBin "" = ""
hexToBin (x:xs) = (hexCharToBin x) ++ (hexToBin xs)


-- find a good way to do this, without reinventing the wheel
hexCharToBin :: HexChar -> BinString
hexCharToBin '0' = "0000"
hexCharToBin '1' = "0001"
hexCharToBin '2' = "0010"
hexCharToBin '3' = "0011"
hexCharToBin '4' = "0100"
hexCharToBin '5' = "0101"
hexCharToBin '6' = "0110"
hexCharToBin '7' = "0111"
hexCharToBin '8' = "1000"
hexCharToBin '9' = "1001"
hexCharToBin 'A' = "1010"
hexCharToBin 'B' = "1011"
hexCharToBin 'C' = "1100"
hexCharToBin 'D' = "1101"
hexCharToBin 'E' = "1110"
hexCharToBin 'F' = "1111"
