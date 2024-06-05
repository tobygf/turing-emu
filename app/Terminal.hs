module Terminal where

import System.IO

data Command = UpArrow | DownArrow | LeftArrow | RightArrow | Other | End

getSequence :: IO Command
getSequence = do
    c1 <- getChar
    if c1 == '['
       then do
           c2 <- getChar
           case c2 of
                'A' -> pure UpArrow
                'B' -> pure DownArrow
                'C' -> pure RightArrow
                'D' -> pure LeftArrow
                _ -> pure Other
       else pure Other

getCommand :: IO Command
getCommand = do
    first <- getChar
    case first of
        '\x1B' -> getSequence
        '\x04' -> pure End
        _ -> pure Other

clearLine :: IO ()
clearLine = do
    putChar '\x1B'
    putStr "[2K"

upLine :: IO ()
upLine = do
    putChar '\x1B'
    putStr "[1A"

reset2 :: IO ()
reset2 = do
    clearLine
    upLine
    clearLine
    cr

cr :: IO ()
cr = putChar '\r'

bell :: IO ()
bell = putChar '\a'
