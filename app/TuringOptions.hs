module TuringOptions where

import Options.Applicative
import Options.Applicative.Extra

data Output = StdOut
            | FileOut String
data Distances = MkDistances Int Int
data Mode = Normal Output Distances
          | Debug
data TuringOptions = MkTuringOptions String String Mode

fileOut :: Parser Output
fileOut = FileOut <$> strOption
    (
        long "output" <>
        short 'o' <>
        metavar "FILE" <>
        value "output_tape.txt" <>
        help "Write output to FILE."
    )

stdout :: Parser Output
stdout = flag' StdOut
    (
        long "stdout" <>
        short 's' <>
        help "Write output to standard output."
    )

output :: Parser Output
output = fileOut <|> stdout

ldistance :: Parser Int
ldistance = option auto
    (
        long "ldist" <>
        short 'l' <>
        metavar "N" <>
        value 10 <>
        help "Output N symbols to the left of the tape head."
    )

rdistance :: Parser Int
rdistance = option auto
    (
        long "rdist" <>
        short 'r' <>
        metavar "N" <>
        value 10 <>
        help "Output N symbols to the right of the tape head."
    )

distances :: Parser Distances
distances = liftA2 MkDistances ldistance rdistance

debug :: Parser Mode
debug = flag' Debug
    (
        long "debug" <>
        short 'd' <>
        help "Run in debug mode."
    )

normal :: Parser Mode
normal = liftA2 Normal output distances

mode :: Parser Mode
mode = debug <|> normal

instructions :: Parser String
instructions = strArgument
    (
        metavar "INSTRUCTIONS" <>
        help "Read instructions from INSTRUCTIONS."
    )

tapeIn :: Parser String
tapeIn = strArgument
    (
        metavar "INPUT_TAPE" <>
        help "Read input tape from INPUT_TAPE."
    )

turingOptions :: Parser TuringOptions
turingOptions = pure MkTuringOptions <*> instructions <*> tapeIn <*> mode

turingHelper :: Parser (a -> a)
turingHelper = helperWith
    (
        long "help" <>
        short 'h' <>
        help "Show this help text." <>
        hidden
    )

desc :: String
desc = "An emulator for turing machines. Provides two modes. \
       \Normal execution mode: writes the final state and output tape \
       \to stdout or to a file. \
       \Interactive debug mode: allows the user to step through the \
       \machine's computation, and naviate left and right along the tape \
       \at all stages. Use flag -d to run in debug mode."

turingOptionsInfo :: ParserInfo TuringOptions
turingOptionsInfo = info (turingOptions <**> turingHelper)
    (
    fullDesc <>
    progDesc desc <>
    header "turing-emu: a turing machine emulator." <>
    footer "This program is free software; you can redistribute it and/or \
           \modify it under the terms of the GNU General Public License as \
           \published by the Free Software Foundation; version 2."
    )
