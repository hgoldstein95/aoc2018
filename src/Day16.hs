module Day16 where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Bits ((.&.), (.|.))
import Data.List (intersect)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf (printf)

import ElfCode.Types
import Day16.Parsers
import Day16.Matching

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

evalProgram :: [Instr] -> Regs
evalProgram = foldl (flip eval) (Array.listArray (0, 3) [0, 0, 0, 0])

validCodes :: Sample -> [Op]
validCodes (Sample input (MachineInstr _ a b c) output) =
  filter (\op -> eval (Instr op a b c) input == output) allOpcodes

countThreeOp :: [Sample] -> Int
countThreeOp = length . filter (>= 3) . fmap (length . validCodes)

computeFacts :: [Sample] -> Map Int [Op]
computeFacts = Map.unionsWith intersect . fmap (uncurry Map.singleton . fact)
  where fact s@(Sample _ (MachineInstr code _ _ _) _) = (code, validCodes s)

disassemble :: Map Int Op -> [MachineInstr] -> [Instr]
disassemble m = fmap (\(MachineInstr op a b c) -> Instr (m Map.! op) a b c)

run :: IO ()
run = do
  samples <- parseSamples <$> readFile "data/Day16-samples.txt"
  printf "Part 1: %d\n" $ countThreeOp samples
  let codeMap = findMatching . computeFacts $ samples
  program <- disassemble codeMap . parseProgram
    <$> readFile "data/Day16-program.txt"
  printf "Part 2: %d\n" $ evalProgram program!0
