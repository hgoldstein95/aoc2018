{-# LANGUAGE TemplateHaskell #-}

module ElfCode.Types where

import Control.Lens (makeLenses)
import Data.Array (Array)

data Op = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti
        | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
        deriving (Show, Eq, Ord)

allOpcodes :: [Op]
allOpcodes = [ Addr, Addi, Mulr, Muli, Banr, Bani, Borr, Bori, Setr, Seti
             , Gtir, Gtri, Gtrr, Eqir, Eqri, Eqrr
             ]

type Regs = Array Int Int

data Instr = Instr
  { _opcode :: Op
  , _inputA :: Int
  , _inputB :: Int
  , _outputC :: Int
  }

data MachineInstr = MachineInstr
  { _miOpcode :: Int
  , _miInputA :: Int
  , _miInputB :: Int
  , _miOutputC :: Int
  }

instance Show MachineInstr where
  show (MachineInstr op a b c) = unwords . fmap show $ [op, a, b, c]

data Sample = Sample
  { _before :: Regs
  , _instr :: MachineInstr
  , _after :: Regs
  }

instance Show Sample where
  show (Sample input mi output) =
    "Before: " ++ show input ++ "\n" ++
    show mi ++ "\n" ++
    "After: " ++ show output ++ "\n"

makeLenses ''MachineInstr
makeLenses ''Instr
makeLenses ''Sample
