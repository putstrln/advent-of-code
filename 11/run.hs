import Text.Megaparsec as MP hiding (State)
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Control.Monad (void)
import Data.Maybe (fromJust)
import Data.Bifunctor (bimap)
import Control.Monad.Trans.State
import Debug.Trace
import Data.List (sort)

data Operation = Multiply | Add | Square deriving (Show)
data Monkey = Monkey {
    rank :: Int,
    items :: [Int],
    operation :: Operation,
    operand :: Maybe Int,
    divisor :: Int,
    business :: Int,
    pairsToThrow :: [(Int, Int)],
    monkeyIndexPredicateSuccess :: Int,
    monkeyIndexPredicateFailure :: Int
} deriving (Show)
type Troop = [Monkey]
type MonkeyIndexItemPair = (Int, Int)

type TroopState = State Troop

type Parser = Parsec Void String

emptyLine :: Parser ()
emptyLine = L.skipLineComment "\n"

sc = L.space space1 emptyLine empty

lexeme = L.lexeme sc
integer = lexeme L.decimal
symb = L.symbol sc
colon = symb ":"

pOperation :: Parser Operation
pOperation = do
    op <- try (symb "* old") <|> symb "*" <|> symb "+"
    case op of
        "* old" -> pure Square
        "*" -> pure Multiply
        "+" -> pure Add
        _ -> error "invalid op"

pMonkey :: Parser Monkey
pMonkey = do
    symb "Monkey"
    rank <- integer
    colon
    --
    symb "Starting items:"
    items <- integer `sepBy` string ", "
    --
    symb "Operation: new = old"
    operation <- pOperation
    operand <- optional integer
    --
    symb "Test: divisible by"
    divisor <- integer
    --
    symb "If true: throw to monkey"
    monkeyIndexPredicateSuccess <- integer
    symb "If false: throw to monkey"
    monkeyIndexPredicateFailure <- integer
    pure $ Monkey {
        rank = rank,
        items = items,
        operation = operation,
        operand = operand,
        divisor = divisor,
        monkeyIndexPredicateSuccess = monkeyIndexPredicateSuccess,
        monkeyIndexPredicateFailure = monkeyIndexPredicateFailure,
        pairsToThrow = [],
        business = 0
    }

pInput :: Parser Troop
pInput = MP.many pMonkey

updateMonkey :: Monkey -> Troop -> Troop
updateMonkey m troop = firstHalf ++ [m] ++ tail secondHalf
    where (firstHalf, secondHalf) = splitAt (rank m) troop

traceState :: (Show a) => a -> State s a
traceState x = state (\s -> trace ("step: " ++ show x ++ "\n") (x, s))

inspectItemsState m manageWorryLevel = do
    troop <- get
    let inspect = manageWorryLevel . op
    let fullItems = items (troop !! rank m)
    let updatedBusiness = business (troop !! rank m) + length fullItems
    let items' = map inspect fullItems
    let pairsToThrow = map (\i -> (nextMonkey i, i)) items'
    troopWithInspectedMonkey <- gets (updateMonkey (m {items = [], pairsToThrow = pairsToThrow, business = updatedBusiness }))
    put (foldl throwItemAcc troopWithInspectedMonkey pairsToThrow)
    t <- get
    -- traceState (troop, items', pairsToThrow, troopWithInspectedMonkey, t)
    pure (troop, items', pairsToThrow, troopWithInspectedMonkey, t)
    where
        inspect = manageWorryLevel . op
        op i = case operation m of
            Square -> i * i
            Multiply -> i * fromJust (operand m)
            Add -> i + fromJust (operand m)
        nextMonkey i =
            if i `rem` divisor m == 0 then monkeyIndexPredicateSuccess m
            else monkeyIndexPredicateFailure m
        throwItemAcc acc (mi, i) = throwItem mi i acc

throwItem :: Int -> Int -> Troop -> Troop
throwItem mi i troop = updateMonkey (m {items = items m ++ [i]}) troop where m = troop !! mi

doRound troop manageWorryLevel = runState (mapM_ (`inspectItemsState` manageWorryLevel) troop) troop

doRounds troop manageWorryLevel = foldl (\(_, t) _ -> doRound t manageWorryLevel) ((), troop)

monkeyBusiness troop manageWorryLevel range = product . take 2 . reverse . sort . map business . snd $ doRounds troop manageWorryLevel range

main = do
    input <- readFile "input"
    case runParser (pInput <* eof) "" input of
        Right troop -> do
            print $ monkeyBusiness troop (`mod` (product $ map divisor troop)) [1..10000]
        Left x -> print x

