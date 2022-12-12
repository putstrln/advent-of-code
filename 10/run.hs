data Command = NoOp | Add Int deriving (Show)
type RegisterValue = Int

parseCommand ["noop"] = [NoOp]
parseCommand ["addx", num] = [NoOp, Add $ (read :: String -> Int) num]
parseCommand _ = error "invalid command"

processCommand NoOp rv  = rv
processCommand (Add n) rv = n + rv

cycles = take 6 (iterate (+40) 20)

main = do
    input <- readFile "input"
    let commandsInput = map words . lines $ input
    let commands = concatMap parseCommand commandsInput
    let results = scanl (flip processCommand) 1 commands
    let cycleResults = map (\cycle -> cycle * results !! (cycle - 1)) cycles
    print $ sum cycleResults
    let pixels = map (zipWith (curry isSpriteBeingDrawn) [0..]) $ chunksOf 40 results
    mapM_ print pixels
    where
        isSpriteBeingDrawn (pixelDrawn, registerValue) =
            if abs (registerValue - pixelDrawn) <= 1 then '#' else '.'
        chunksOf _ [] = []
        chunksOf n l = take n l : chunksOf n (drop n l)