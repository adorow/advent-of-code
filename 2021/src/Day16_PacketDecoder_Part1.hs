----- Day 16: Packet Decoder ---
--As you leave the cave and reach open waters, you receive a transmission from the Elves back on the ship.
--
--The transmission was sent using the Buoyancy Interchange Transmission System (BITS), a method of packing numeric expressions into a binary sequence. Your submarine's computer has saved the transmission in hexadecimal (your puzzle input).
--
--The first step of decoding the message is to convert the hexadecimal representation into binary. Each character of hexadecimal corresponds to four bits of binary data:
--
--0 = 0000
--1 = 0001
--2 = 0010
--3 = 0011
--4 = 0100
--5 = 0101
--6 = 0110
--7 = 0111
--8 = 1000
--9 = 1001
--A = 1010
--B = 1011
--C = 1100
--D = 1101
--E = 1110
--F = 1111
--The BITS transmission contains a single packet at its outermost layer which itself contains many other packets. The hexadecimal representation of this packet might encode a few extra 0 bits at the end; these are not part of the transmission and should be ignored.
--
--Every packet begins with a standard header: the first three bits encode the packet version, and the next three bits encode the packet type ID. These two values are numbers; all numbers encoded in any packet are represented as binary with the most significant bit first. For example, a version encoded as the binary sequence 100 represents the number 4.
--
--Packets with type ID 4 represent a literal value. Literal value packets encode a single binary number. To do this, the binary number is padded with leading zeroes until its length is a multiple of four bits, and then it is broken into groups of four bits. Each group is prefixed by a 1 bit except the last group, which is prefixed by a 0 bit. These groups of five bits immediately follow the packet header. For example, the hexadecimal string D2FE28 becomes:
--
--110100101111111000101000
--VVVTTTAAAAABBBBBCCCCC
--Below each bit is a label indicating its purpose:
--
--The three bits labeled V (110) are the packet version, 6.
--The three bits labeled T (100) are the packet type ID, 4, which means the packet is a literal value.
--The five bits labeled A (10111) start with a 1 (not the last group, keep reading) and contain the first four bits of the number, 0111.
--The five bits labeled B (11110) start with a 1 (not the last group, keep reading) and contain four more bits of the number, 1110.
--The five bits labeled C (00101) start with a 0 (last group, end of packet) and contain the last four bits of the number, 0101.
--The three unlabeled 0 bits at the end are extra due to the hexadecimal representation and should be ignored.
--So, this packet represents a literal value with binary representation 011111100101, which is 2021 in decimal.
--
--Every other type of packet (any packet with a type ID other than 4) represent an operator that performs some calculation on one or more sub-packets contained within. Right now, the specific operations aren't important; focus on parsing the hierarchy of sub-packets.
--
--An operator packet contains one or more packets. To indicate which subsequent binary data represents its sub-packets, an operator packet can use one of two modes indicated by the bit immediately after the packet header; this is called the length type ID:
--
--If the length type ID is 0, then the next 15 bits are a number that represents the total length in bits of the sub-packets contained by this packet.
--If the length type ID is 1, then the next 11 bits are a number that represents the number of sub-packets immediately contained by this packet.
--Finally, after the length type ID bit and the 15-bit or 11-bit field, the sub-packets appear.
--
--For example, here is an operator packet (hexadecimal string 38006F45291200) with length type ID 0 that contains two sub-packets:
--
--00111000000000000110111101000101001010010001001000000000
--VVVTTTILLLLLLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBBBBBB
--The three bits labeled V (001) are the packet version, 1.
--The three bits labeled T (110) are the packet type ID, 6, which means the packet is an operator.
--The bit labeled I (0) is the length type ID, which indicates that the length is a 15-bit number representing the number of bits in the sub-packets.
--The 15 bits labeled L (000000000011011) contain the length of the sub-packets in bits, 27.
--The 11 bits labeled A contain the first sub-packet, a literal value representing the number 10.
--The 16 bits labeled B contain the second sub-packet, a literal value representing the number 20.
--After reading 11 and 16 bits of sub-packet data, the total length indicated in L (27) is reached, and so parsing of this packet stops.
--
--As another example, here is an operator packet (hexadecimal string EE00D40C823060) with length type ID 1 that contains three sub-packets:
--
--11101110000000001101010000001100100000100011000001100000
--VVVTTTILLLLLLLLLLLAAAAAAAAAAABBBBBBBBBBBCCCCCCCCCCC
--The three bits labeled V (111) are the packet version, 7.
--The three bits labeled T (011) are the packet type ID, 3, which means the packet is an operator.
--The bit labeled I (1) is the length type ID, which indicates that the length is a 11-bit number representing the number of sub-packets.
--The 11 bits labeled L (00000000011) contain the number of sub-packets, 3.
--The 11 bits labeled A contain the first sub-packet, a literal value representing the number 1.
--The 11 bits labeled B contain the second sub-packet, a literal value representing the number 2.
--The 11 bits labeled C contain the third sub-packet, a literal value representing the number 3.
--After reading 3 complete sub-packets, the number of sub-packets indicated in L (3) is reached, and so parsing of this packet stops.
--
--For now, parse the hierarchy of the packets throughout the transmission and add up all of the version numbers.
--
--Here are a few more examples of hexadecimal-encoded transmissions:
--
--8A004A801A8002F478 represents an operator packet (version 4) which contains an operator packet (version 1) which contains an operator packet (version 5) which contains a literal value (version 6); this packet has a version sum of 16.
--620080001611562C8802118E34 represents an operator packet (version 3) which contains two sub-packets; each sub-packet is an operator packet that contains two literal values. This packet has a version sum of 12.
--C0015000016115A2E0802F182340 has the same structure as the previous example, but the outermost packet uses a different length type ID. This packet has a version sum of 23.
--A0016C880162017C3686B18A3D4780 is an operator packet that contains an operator packet that contains an operator packet that contains five literal values; it has a version sum of 31.
--Decode the structure of your hexadecimal-encoded BITS transmission; what do you get if you add up the version numbers in all packets?

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

data Expression = Literal Int | Operation [Packet] deriving (Show)
data Packet = Packet {
    version :: Version,
    expression :: Expression
} deriving (Show)

-- maybe can turn ParsedWithLeftover into a Monad to simplify some things?
type ParsedWithLeftover a = (a, BinString)

main = do
    withFile "./2021/resources/day16.in" ReadMode (\handle -> do
        contents <- hGetContents handle
        let (outerPacket, leftover) = decodePacket $ hexToBin contents
--        let result = decodePacket $ "110100101111111000101000" -- packet with literal (version 6, value 2021), 3 zero-bits are left over
--        let result = decodePacket $ "00111000000000000110111101000101001010010001001000000000" -- packet with length in bits (version 1, type ID 6, 27 bits of content:> 11 bits with literal 10, 16 bits with literal 20), 7 zero-bits are left over
--        let result = decodePacket $ "11101110000000001101010000001100100000100011000001100000" -- packet with n sub-packets (version 7, type ID 3, 3 sub-packets :> 11 bits with literal 1, 11 bits with literal 2, 11 bits with literal 3), 5 zero-bits are left over
        putStrLn $ show outerPacket
        putStrLn $ show leftover
        putStrLn (show $ sumPacketVersions outerPacket))

decodePacket :: BinString -> ParsedWithLeftover Packet
decodePacket bin = (Packet { version = version, expression = expression}, leftover)
    where version = binToInt (take 3 bin)
          typeId = binToInt (take 3 . drop 3 $ bin)
          remainingBin = (drop 6 bin)
          (expression, leftover) = decodeExpression typeId remainingBin

decodeExpression :: Int -> BinString -> ParsedWithLeftover Expression
-- typeId=4 => literal
decodeExpression 4 bin = decodeLiteral bin
-- any other => operation
decodeExpression _ bin = decodeOperation bin


decodeLiteral :: String -> ParsedWithLeftover Expression
decodeLiteral bin = (Literal (binToInt binLiteral), leftover)
    where (binLiteral, leftover) = readLiteralBin bin


-- this function reads the input binary (sequence of 5 packet digits, where the first is a flag dictating if there are more packets of 5 to read)
readLiteralBin :: BinString -> ParsedWithLeftover BinString
readLiteralBin ('0':xs) = ((take 4 xs), drop 4 xs) -- take the first 4, the rest is leftover)
readLiteralBin ('1':xs) = ((take 4 xs) ++ bin, leftover)
    where (bin, leftover) = readLiteralBin (drop 4 xs)


decodeOperation :: BinString -> ParsedWithLeftover Expression
-- type=0 => next 15 bits are the total length in bits
-- type=1 => next 11 bits are the number of sub-packets immediately contained
decodeOperation ('0':xs) = decodeOperationOfBitSize numberOfBitsSubPackets remaining
    where numberOfBitsSubPackets = binToInt (take 15 xs)
          remaining = (drop 15 xs)
decodeOperation ('1':xs) = decodeOperationOfPacketLength numberSubPackets remaining
    where numberSubPackets = binToInt (take 11 xs)
          remaining = (drop 11 xs)


decodeOperationOfBitSize :: Int -> BinString -> ParsedWithLeftover Expression
decodeOperationOfBitSize size bin = (Operation packets, leftover)
    where (packets, leftover) = decodePacketsOfBitSize size bin


decodeOperationOfPacketLength :: Int -> BinString -> ParsedWithLeftover Expression
decodeOperationOfPacketLength numPackets bin = (Operation packets, leftover)
    where (packets, leftover) = decodePacketsOfLength numPackets bin

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


sumPacketVersions :: Packet -> Int
sumPacketVersions Packet { version = version, expression = expression } = version + (sumExpressionVersions expression)


sumExpressionVersions :: Expression -> Int
sumExpressionVersions (Literal _) = 0
sumExpressionVersions (Operation packets) = sum $ map sumPacketVersions packets
