module ParseTuring where

import Turing

import Data.Char
import qualified Text.Read as TR
import Data.List

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

-- Format
-- q0,'S','T',R,q1;

data ParseStatus = MkParseStatus {
    parseRemaining :: String,
    parseLine :: Integer
}

type Parse a = ExceptT ParseError (State ParseStatus) a

data ParseError = MkParseError Integer Message

data Message = UnexpectedEOF | WrongChar [Char] Char | EmptyStateName | BadSymbolParse

showCharList :: [Char] -> String
showCharList chars = "[" ++
                     intercalate ", " (fmap show chars) ++
                     "]"

messageFeedback :: Message -> String
messageFeedback UnexpectedEOF = "Unexpected end-of-file."
messageFeedback (WrongChar [good_c] bad_c) = "Expected character " ++ show good_c ++
    ", got character " ++ show bad_c ++ " instead."
messageFeedback (WrongChar good_cs bad_c) = "Expected one of " ++ showCharList good_cs ++
    ", got " ++ show bad_c ++ " instead."
messageFeedback EmptyStateName = "State name is empty."
messageFeedback BadSymbolParse = "Failed to parse normal symbol."

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\t' = True
isWhitespace '\n' = True
isWhitespace _ = False

getParseRemaining :: Parse String
getParseRemaining = fmap parseRemaining (lift get)
getParseLine :: Parse Integer
getParseLine = fmap parseLine (lift get)
putParseRemaining :: String -> Parse ()
putParseRemaining new = do
    status <- lift get
    lift $ put status {parseRemaining = new}
putParseLine :: Integer -> Parse ()
putParseLine new = do
    status <- lift get
    lift $ put status {parseLine = new}

report :: Message -> Parse a
report msg = do
    line <- getParseLine
    throwE (MkParseError line msg)

inc :: Parse ()
inc = do
    l <- getParseLine
    putParseLine (l + 1)

isParseEOF :: Parse Bool
isParseEOF = fmap null getParseRemaining

advance :: Parse Char
advance = do
    remaining <- getParseRemaining
    case remaining of
         c:cs -> do
             when (c == '\n') inc
             putParseRemaining cs
             pure c
         "" -> report UnexpectedEOF

advanceMany :: Integer -> Parse String
advanceMany i
    | i == 0 = pure ""
    | i > 0 = do
        c <- advance
        rest <- advanceMany (i - 1)
        pure (c:rest)
    | i < 0 = error "eatMany has gone haywire"

advanceWhile :: (Char -> Bool) -> Parse String
advanceWhile p = do
    next <- peek
    case next of
         Nothing -> pure ""
         Just c
             | p c -> do
                 advance
                 rest <- advanceWhile p
                 pure (c:rest)
             | otherwise -> pure ""

eat :: Char -> Parse ()
eat correct_c = do
    c <- advance
    if c == correct_c
       then pure ()
       else report (WrongChar [correct_c] c)

eatMany :: String -> Parse ()
eatMany (c:cs) = do
    eat c
    eatMany cs

eatMany "" = pure ()

peek :: Parse (Maybe Char)
peek = do
    remaining <- getParseRemaining
    case remaining of
         c:cs -> pure (Just c)
         "" -> pure Nothing

skipWhitespace :: Parse ()
skipWhitespace = do
    next <- peek
    case next of
         Just c -> when (isWhitespace c) $ advance >> skipWhitespace
         Nothing -> pure ()

eatWithWhitespace :: Char -> Parse ()
eatWithWhitespace c = do
    skipWhitespace
    eat c
    skipWhitespace

parseState :: Parse MachineState
parseState = do
    next <- peek
    case next of
         Nothing -> report UnexpectedEOF
         Just 'q' -> do
             advance
             name <- advanceWhile isAlphaNum
             if null name
                then report EmptyStateName
                else pure $ NormalState name
         Just 'H' -> eatMany "HALT" >> pure Halt
         Just other -> report $ WrongChar ['q', 'H'] other

printState :: MachineState -> String
printState Halt = "HALT"
printState (NormalState name) = 'q':name

parseSymbol :: Parse TapeSymbol
parseSymbol = do
    next <- peek
    case next of
         Nothing -> report UnexpectedEOF
         Just 'b' -> advance >> pure Blank
         Just '\'' -> do
             attempt <- fmap TR.reads getParseRemaining
             case attempt of
                  (symbolChar, rest):xs -> do
                      putParseRemaining rest
                      pure $ NormalSymbol symbolChar
                  [] -> report BadSymbolParse
         Just other -> report (WrongChar ['b', '\''] other)

parseSymbols :: Parse [TapeSymbol]
parseSymbols = do
    skipWhitespace
    symbol <- parseSymbol
    skipWhitespace
    next <- peek
    case next of
         Nothing -> pure [symbol]
         Just ',' -> do
             advance
             rest <- parseSymbols
             pure (symbol:rest)
         _ -> pure [symbol]

printSymbol :: TapeSymbol -> String
printSymbol (NormalSymbol c) = show c
printSymbol Blank = "b"

printSymbols :: [TapeSymbol] -> String
printSymbols symbols = intercalate ", " (fmap printSymbol symbols)

parseDirection :: Parse Direction
parseDirection = do
    next <- peek
    case next of
         Nothing -> report UnexpectedEOF
         Just 'L' -> advance >> pure L
         Just 'N' -> advance >> pure N
         Just 'R' -> advance >> pure R
         Just other -> report (WrongChar ['L', 'N', 'R'] other)

parseInstruction :: Parse Instruction
parseInstruction = do
    skipWhitespace
    state1 <- parseState
    eatWithWhitespace ','
    symbol1 <- parseSymbol
    eatWithWhitespace ','
    symbol2 <- parseSymbol
    eatWithWhitespace ','
    dir <- parseDirection
    eatWithWhitespace ','
    state2 <- parseState

    eatWithWhitespace ';'

    let result = MkInstruction state1 symbol1 symbol2 dir state2
    pure result

parseInstructions :: Parse [Instruction]
parseInstructions = do
    skipWhitespace
    eof <- isParseEOF
    if eof
       then pure []
       else do
           instruction <- parseInstruction
           rest <- parseInstructions
           pure (instruction:rest)

infinify :: [TapeSymbol] -> [TapeSymbol]
infinify symbols = symbols ++ repeat Blank

parseTape :: Parse MachineTape
parseTape = do
    skipWhitespace
    symbolsLeft <- parseSymbols
    eatWithWhitespace ';'
    th <- parseSymbol
    eatWithWhitespace ';'
    symbolsRight <- parseSymbols
    skipWhitespace

    let tl = infinify (reverse symbolsLeft)
    let tr = infinify symbolsRight

    pure $ MkMachineTape tl th tr

startParse :: Parse a -> String -> Either ParseError a
startParse parser toParse =
    let start = MkParseStatus toParse 1
        s = runExceptT parser
        result = evalState s start
    in result

printTape :: Int -> Int -> MachineTape -> String
printTape ldistance rdistance tape =
    let tl = tapeLeft tape
        th = tapeHead tape
        tr = tapeRight tape

        toPrintLeft = reverse (take ldistance tl)
        toPrintRight = take rdistance tr
    in "..., " ++ printSymbols toPrintLeft ++ " ; " ++
       printSymbol th ++ " ; " ++
       printSymbols toPrintRight ++ ", ..."

printStatus :: Int -> Int -> MachineStatus -> String
printStatus ldistance rdistance (MkMachineStatus tape mState) =
    "State: " ++ printState mState ++ "\n" ++
    printTape ldistance rdistance tape

printMachineError :: MachineError -> String
printMachineError (MkNMIError mState symbol) =
    "No instruction matches state: " ++ printState mState ++ " and symbol on head: " ++ printSymbol symbol
