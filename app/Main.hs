module Main where

import Turing
import ParseTuring

import Terminal

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Data.List
import Data.Foldable
import Data.Either
import System.IO
import Control.Exception

import Options.Applicative (execParser)
import qualified TuringOptions as TO

import qualified Data.Vector as V
import Data.Vector (Vector, (!), fromList)

displaySymbol :: TapeSymbol -> String
displaySymbol Blank = " b "
displaySymbol other = printSymbol other

displayTape :: Int -> MachineTape -> String
displayTape center tape =
    let toDisplayLeft = drop (-center - 12) (take (10 - center) (tapeLeft tape))
        toDisplayRight = drop (center - 12) (take (10 + center) (tapeRight tape))
        shouldDisplayHead = (center < 11 && center > -11)
    in
        concatMap ((++ " ") . displaySymbol) (reverse toDisplayLeft) ++
        (if shouldDisplayHead
            then "[" ++ displaySymbol (tapeHead tape) ++ "]"
            else "") ++
        concatMap ((" " ++) . displaySymbol) toDisplayRight

execute :: [Instruction] -> MachineTape -> Int -> Int -> (String, Bool)
execute instructions tapeIn ldist rdist =
    let (end, diag) = run' instructions (MkMachineStatus tapeIn (NormalState "0"))
        output = printStatus ldist rdist end
    in case diag of
         Halted -> (output, False)
         Errored e -> (output ++ "\nAfter this, the machine encountered an error:\n" ++
            printMachineError e, True)
         StillRunning -> error "We shouldn't get StillRunning here!"

data DebugScreen = StatusScreen MachineStatus | OtherScreen String

putDebugScreen :: Int -> DebugScreen -> IO ()
putDebugScreen center (StatusScreen machineStatus) = do
    let mState = machineState machineStatus
    let displayedTape = displayTape center (machineTape machineStatus)
    putStr $ "State: " ++ printState mState ++ "\n" ++ displayedTape
putDebugScreen center (OtherScreen str) = putStr str

endDebugLoop :: IO ()
endDebugLoop = do
    putChar '\n'
    pure ()

debugLoop :: Vector DebugScreen -> Int -> Int -> IO ()
-- i for screen no., j for center/horizontal navigation
debugLoop screens i j = do
    when (i < 0) $ error "WRONG INDEX"
    when (i >= length screens) $ error "WRONG INDEX"

    command <- getCommand
    case command of
         UpArrow ->
            if i > 0
                then do
                    reset2
                    putDebugScreen j (screens ! (i - 1))
                    debugLoop screens (i - 1) j
                else do
                    bell
                    debugLoop screens i j
         DownArrow ->
            if i < (length screens - 1)
                then do
                    reset2
                    putDebugScreen j (screens ! (i + 1))
                    debugLoop screens (i + 1) j
                else do
                    bell
                    debugLoop screens i j
         LeftArrow -> do
             reset2
             putDebugScreen (j - 1) (screens ! i)
             debugLoop screens i (j - 1)
         RightArrow -> do
             reset2
             putDebugScreen (j + 1) (screens ! i)
             debugLoop screens i (j + 1)
         End -> endDebugLoop
         _ -> debugLoop screens i j


debug :: [Instruction] -> MachineTape -> IO ()
debug instructions tapeIn = do
    let (statuses, diag) = run instructions (MkMachineStatus tapeIn (NormalState "0"))
    let statusScreens = fmap StatusScreen statuses
    let screens = case diag of
                       Halted -> statusScreens
                       Errored e -> let final = "Encountered an error:\n" ++ printMachineError e
                                    in statusScreens ++ [OtherScreen final]
                       StillRunning -> error "We shouldn't get StillRunning here!"
    let screensVect = fromList screens :: Vector DebugScreen

    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    hSetEcho stdin False

    putDebugScreen 0 (screensVect ! 0)
    debugLoop screensVect 0 0

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft f (Right y) = Right y

instructionsHelper :: String -> String -> Either String [Instruction]
instructionsHelper source filename =
    let result = startParse parseInstructions source
        fn (MkParseError line msg) = "Error parsing instructions from " ++ filename ++ ":\n" ++
                                      "line " ++ show line ++ ": " ++ messageFeedback msg
    in mapLeft fn result

tapeHelper :: String -> String -> Either String MachineTape
tapeHelper source filename =
    let result = startParse parseTape source
        fn (MkParseError line msg) = "Error parsing input tape from " ++ filename ++ ":\n" ++
                                     "line " ++ show line ++ ": " ++ messageFeedback msg
    in mapLeft fn result

normalizeHelper :: Maybe String -> IOException -> String
normalizeHelper (Just preamble) e = preamble ++ ":\n" ++ show e
normalizeHelper Nothing e = show e

normalize :: Maybe String -> Either IOException a -> Either String a
normalize maybePreamble = mapLeft (normalizeHelper maybePreamble)

process :: Either IOException String -> Either IOException String -> String -> String -> Either String ([Instruction], MachineTape)
process instructionsFileTry tapeInFileTry instructionsFilename tapeInFilename = do
    instructionsFile <- normalize (Just ("Error reading from file \"" ++ instructionsFilename ++ "\"")) instructionsFileTry
    tapeInFile <- normalize (Just ("Error reading from file \"" ++ tapeInFilename ++ "\"")) tapeInFileTry

    instructions <- instructionsHelper instructionsFile instructionsFilename
    tapeIn <- tapeHelper tapeInFile tapeInFilename

    pure (instructions, tapeIn)

executeStdout :: [Instruction] -> MachineTape -> Int -> Int -> IO ()
executeStdout instructions tapeIn ldist rdist =
    let (output, _) = execute instructions tapeIn ldist rdist
    in putStrLn output

executeFileOut :: String -> [Instruction] -> MachineTape -> Int -> Int -> IO ()
executeFileOut filename instructions tapeIn ldist rdist = do
    let (output, errored) = execute instructions tapeIn ldist rdist
    tapeWriteTry <- try $ writeFile filename (output ++ "\n")
    let normalized = normalize (Just ("Error writing to file \"" ++ filename ++ "\"")) tapeWriteTry
    case normalized of
         Left exceptionStr -> putStrLn exceptionStr
         Right () -> when errored $ putStrLn ("The turing machine encountered a runtime error. Details are in " ++ filename)

strictReadFile :: String -> IO String
strictReadFile filename = do
    contents <- readFile filename
    seq (length contents) (pure contents)

main :: IO ()
main = do
    options <- execParser TO.turingOptionsInfo
    let TO.MkTuringOptions instructionsFilename tapeInFilename mode = options

    instructionsFileTry <- try $ strictReadFile instructionsFilename
    tapeInFileTry <- try $ strictReadFile tapeInFilename

    let dataEither = process instructionsFileTry tapeInFileTry instructionsFilename tapeInFilename

    case dataEither of
         Left errorString -> putStrLn errorString
         Right (instructions, tapeIn) ->
            case mode of
                 TO.Debug -> debug instructions tapeIn
                 TO.Normal TO.StdOut (TO.MkDistances ldist rdist) -> executeStdout instructions tapeIn ldist rdist
                 TO.Normal (TO.FileOut tapeOutFilename) (TO.MkDistances ldist rdist) -> executeFileOut tapeOutFilename instructions tapeIn ldist rdist
