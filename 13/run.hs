{-# LANGUAGE BlockArguments #-}
import Text.Megaparsec as MP hiding (State)
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Text.Parsec (anyChar)
import Data.List.Split (chunksOf)
import Control.Monad (guard)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)

type Parser = Parsec Void String

emptyLine :: Parser ()
emptyLine = L.skipLineComment "\n"

sc = L.space space1 emptyLine empty

lexeme = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc

data Packet = PacketList [Packet] | Bit Int deriving (Show, Eq)

instance Ord Packet where
    compare (Bit x) (Bit y) = compare x y
    compare (PacketList x) (Bit y) = compare (PacketList x) (PacketList [Bit y])
    compare (Bit x) (PacketList y) = compare (PacketList [Bit x]) (PacketList y)
    compare (PacketList x) (PacketList y) = mconcat (zipWith compare x y) <> compare (length x) (length y)

pBit = Bit <$> dbg "intger" integer

pPacket = do
    brackets <- dbg "brackets" $ between (symb "[") (symb "]") (dbg "pPacket" pPacket' `sepBy` char ',')
    pure $ PacketList brackets
    where
        pPacket' = pPacket
            <|> pBit

pInput = MP.many (dbg "pInput" pPacket)

indexedPairs = zip [1..] . chunksOf 2

process1 packets = sum . map fst . filter (\(i, pp) -> head pp < last pp) $ indexedPairs packets

process2 packets = index p2 * index p6
    where
        p2 = PacketList [PacketList [Bit 2]]
        p6 = PacketList [PacketList [Bit 6]]
        sortedPackets = sort $ p2 : p6 : packets
        index p = fromJust (p `elemIndex` sortedPackets) + 1

main = do
    input <- readFile "input"
    case runParser (pInput <* eof) "" input of
        Right packets -> do
            print $ sequence [process1, process2] packets
        Left x -> print x