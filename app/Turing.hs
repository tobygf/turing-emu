module Turing where

import Control.Monad.Trans.State
import Control.Applicative

data MachineStatus = MkMachineStatus {
    machineTape :: MachineTape,
    machineState :: MachineState
}

data MachineTape = MkMachineTape {
    tapeLeft :: [TapeSymbol],
    tapeHead :: TapeSymbol,
    tapeRight :: [TapeSymbol]
}
data TapeSymbol = Blank | NormalSymbol Char
    deriving Eq

data MachineState = Halt | NormalState String
    deriving Eq

data Direction = L | N | R

data Instruction = MkInstruction {
    initialState :: MachineState,
    initialSymbol :: TapeSymbol,
    symbolToWrite :: TapeSymbol,
    direction :: Direction,
    nextState :: MachineState
}

-- NMI error = no matching instruction error
data MachineError = MkNMIError MachineState TapeSymbol

data MachineDiagnostic = StillRunning | Halted | Errored MachineError

getTape :: State MachineStatus MachineTape
getTape = fmap machineTape get
getTapeLeft :: State MachineStatus [TapeSymbol]
getTapeLeft = fmap tapeLeft getTape
getTapeHead :: State MachineStatus TapeSymbol
getTapeHead = fmap tapeHead getTape
getTapeRight :: State MachineStatus [TapeSymbol]
getTapeRight = fmap tapeRight getTape
getState :: State MachineStatus MachineState
getState = fmap machineState get

putTape :: MachineTape -> State MachineStatus ()
putTape tape = do
    old <- get
    put old {machineTape = tape}
putTapeLeft :: [TapeSymbol] -> State MachineStatus ()
putTapeLeft symbols = do
    oldTape <- getTape
    putTape oldTape {tapeLeft = symbols}
putTapeHead :: TapeSymbol -> State MachineStatus ()
putTapeHead symbol = do
    oldTape <- getTape
    putTape oldTape {tapeHead = symbol}
putTapeRight :: [TapeSymbol] -> State MachineStatus ()
putTapeRight symbols = do
    oldTape <- getTape
    putTape oldTape {tapeRight = symbols}
putState :: MachineState -> State MachineStatus ()
putState s = do
    old <- get
    put old {machineState = s}

liftedCons :: Applicative f => f a -> f [a] -> f [a]
liftedCons = liftA2 (:)

move :: Direction -> State MachineStatus ()
move L = do
    newTapeLeft <- fmap tail getTapeLeft
    newTapeHead <- fmap head getTapeLeft
    newTapeRight <- liftedCons getTapeHead getTapeRight
    let newTape = MkMachineTape newTapeLeft newTapeHead newTapeRight
    putTape newTape
move N = pure ()
move R = do
    newTapeLeft <- liftedCons getTapeHead getTapeLeft
    newTapeHead <- fmap head getTapeRight
    newTapeRight <- fmap tail getTapeRight
    let newTape = MkMachineTape newTapeLeft newTapeHead newTapeRight
    putTape newTape

pertains :: Instruction -> State MachineStatus Bool
pertains instruction = do
    isCorrectSymbol <- fmap (== initialSymbol instruction) getTapeHead
    isCorrectState <- fmap (== initialState instruction) getState
    pure $ isCorrectSymbol && isCorrectState

findPertaining :: [Instruction] -> State MachineStatus (Maybe Instruction)
findPertaining (i:is) = do
    i_pertains <- pertains i
    if i_pertains
        then pure $ Just i
        else findPertaining is
findPertaining [] = pure Nothing

executeInstruction :: Instruction -> State MachineStatus ()
executeInstruction instruction = do
    putTapeHead (symbolToWrite instruction)
    move (direction instruction)
    putState (nextState instruction)

progress :: [Instruction] -> State MachineStatus MachineDiagnostic
progress instructions = do
    i <- findPertaining instructions
    case i of
         Just instruction -> do
             executeInstruction instruction
             currentState <- getState
             case currentState of
                  Halt -> pure Halted
                  _ -> pure StillRunning
         Nothing -> fmap Errored (liftA2 MkNMIError getState getTapeHead)

run :: [Instruction] -> MachineStatus -> ([MachineStatus], MachineDiagnostic)
run instructions start =
    let (diag, next) = runState (progress instructions) start
    in case diag of
            StillRunning ->
                let rest = run instructions next
                    restStatuses = fst rest
                    termination = snd rest
                in (start:restStatuses, termination)

            Halted -> ([start, next], Halted)
            Errored e -> ([start], Errored e)

run' :: [Instruction] -> MachineStatus -> (MachineStatus, MachineDiagnostic)
run' instructions start =
    let (diag, next) = runState (progress instructions) start
    in case diag of
            StillRunning -> run' instructions next
            Halted -> (next, Halted)
            Errored e -> (start, Errored e)
