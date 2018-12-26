module Day19 where

import Data.Array ((!))
import qualified Data.Array as Array
import Data.Bits ((.&.), (.|.))

import ElfCode.Types

-- From Day 16
eval :: Instr -> Regs -> Regs
eval (Instr op a b c) regs = setC $ case op of
  Addr -> regs!a + regs!b
  Addi -> regs!a + b
  Mulr -> regs!a * regs!b
  Muli -> regs!a * b
  Banr -> regs!a .&. regs!b
  Bani -> regs!a .&. b
  Borr -> regs!a .|. regs!b
  Bori -> regs!a .|. b
  Setr -> regs!a
  Seti -> a
  Gtir -> if a > regs!b then 1 else 0
  Gtri -> if regs!a > b then 1 else 0
  Gtrr -> if regs!a > regs!b then 1 else 0
  Eqir -> if a == regs!b then 1 else 0
  Eqri -> if regs!a == b then 1 else 0
  Eqrr -> if regs!a == regs!b then 1 else 0
  where setC v = regs Array.// [(c, v)]

run :: IO ()
run = putStrLn "Day 19 not implemented"
