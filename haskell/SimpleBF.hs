module SimpleBF where
import Data.Word
import Data.Char

type Cell = Word8
data BFState = BFState {
    prog :: [Char],
    tape :: [Cell],
    curProg :: Int,
    curTape :: Int,
    lenProg :: Int,
    debug :: Bool
}

initState :: Int -> [Char] -> BFState
initState capTape prog = BFState {
    prog=prog, tape=(take capTape $ repeat 0), 
    curProg = 0, curTape = 0, lenProg = length(prog), debug = False}

step :: BFState -> IO BFState
step st = case elem of
    '+' -> return st {tape = chngVal st (+1), curProg = (curProg st) + 1}
    '-' -> return st {tape = chngVal st (subtract 1), curProg = (curProg st) + 1}
    '>' -> return st {curTape = (curTape st) + 1, curProg = (curProg st) + 1}
    '<' -> return st {curTape = (curTape st) - 1, curProg = (curProg st) + 1}
    '[' -> return st {curProg = (opnLoop st)}
    ']' -> return st {curProg = (clsLoop st)}
    '.' -> writeVal st 
    ',' -> readVal st
    '#' -> return st {debug = True, curProg = (curProg st) + 1}

    _   -> return st {curProg = (curProg st) + 1}

    where elem = (prog st) !! (curProg st)

chngVal :: BFState -> (Cell -> Cell) -> [Cell]
chngVal st f = putVal (tape st) (curTape st) (f $ getVal st)

putVal :: [Cell] -> Int -> Cell -> [Cell]
putVal tape idx newVal = befTape ++ [newVal] ++ (tail aftTape)
                        where (befTape, aftTape) = splitAt idx tape 
getVal :: BFState -> Cell
getVal st = (tape st) !!  (curTape st)

opnLoop :: BFState -> Int
opnLoop st  = case (getVal st) of
    0 -> opnLoop' (prog st) ((curProg st)+1) 0
    _ -> (curProg st) + 1

opnLoop' prog idx lvl = case (prog !! idx) of 
    '[' -> opnLoop' prog (idx+1) (lvl+1)
    ']' -> if lvl == 0 then idx + 1 else opnLoop' prog (idx+1) (lvl-1)
    _   -> opnLoop' prog (idx+1) lvl

clsLoop :: BFState -> Int
clsLoop st = clsLoop' (prog st) ((curProg st)-1) 0 
clsLoop' prog idx lvl = case (prog !! idx) of
    ']' -> clsLoop' prog (idx-1) (lvl+1)
    '[' -> if lvl == 0 then idx else clsLoop' prog (idx-1) (lvl-1)
    _   -> clsLoop' prog (idx-1) lvl

writeVal :: BFState -> IO BFState
writeVal st =   (putChar . chr . fromEnum . getVal) st >>
                return st {curProg = (curProg st) + 1}

readVal :: BFState -> IO BFState
readVal st = do
    c <- getChar
    let val = (fromIntegral . fromEnum) c
        in return st {tape = (putVal (tape st) (curTape st) val), curProg = (curProg st) + 1}

isEnd :: BFState -> Bool
isEnd st = (curProg st) >= (lenProg st)

run :: BFState -> IO ()
run st = do
    if (debug st) 
        then putStrLn $ show ((curTape st), (curProg st), (tape st)) 
        else return ()

    if isEnd st 
        then return ()
        else step st >>= \st' -> run st'

main :: IO ()
main = do
    run $ initState 10 "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>+\
          \+.>+.+++++++..+++.>++.<<+++++++++++++++.>.++\
          \+.------.--------.>+.>.<"