module AdventOfCode.Intcode (
  Computer(..),
  Opts(..),
  computer,
  continue,
  continueDefault,
  defaults,
  memRead,
  memWrite,
  readInputFileOrIntcode,
  writes,
) where

import Data.Array.IArray ((!), listArray)
import Data.Array.Unboxed (UArray)
import Data.Maybe (fromMaybe)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import System.Environment (getArgs)

data Computer = Computer {
    pos :: Int
  , romem :: UArray Int Int
  , rwmem :: IntMap Int
  , halt :: Bool
  }

data Opts = Opts {
  }

defaults :: Opts
defaults = Opts {
  }

computer :: [Int] -> Computer
computer mem = Computer {
    pos = 0
  , romem = listArray (0, length mem - 1) mem
  , rwmem = IntMap.empty
  , halt = False
  }

data Op =
    Halt
  | Binop (Int -> Int -> Int)

intToOp :: Int -> Op
intToOp 99 = Halt
intToOp 1 = Binop (+)
intToOp 2 = Binop (*)
intToOp code = error ("unknown opcode " ++ show code)

numParams :: Op -> Int
numParams Halt = 0
numParams (Binop _) = 3

numOutputs :: Op -> Int
numOutputs (Binop _) = 1
numOutputs _ = 0

continue :: Opts -> Computer -> Computer
continue _ = until halt step

continueDefault :: Computer -> Computer
continueDefault = continue defaults

memRead :: Int -> Computer -> Int
memRead addr Computer { romem = ro, rwmem = rw } =
  fromMaybe (ro ! addr) (IntMap.lookup addr rw)

memWrite :: Int -> Int -> Computer -> Computer
memWrite addr val comp = comp { rwmem = IntMap.insert addr val (rwmem comp) }

writes :: Computer -> [(IntMap.Key, Int)]
writes = IntMap.toList . rwmem

step :: Computer -> Computer
step comp = let opcode = memRead (pos comp) comp in
  step' (intToOp opcode) comp

step' :: Op -> Computer -> Computer
step' op comp =
  let values = [memRead (pos comp + n) comp | n <- [1 .. numParams op]]
      numInputs = numParams op - numOutputs op
      resolved = map (`memRead` comp) (take numInputs values)
      pos' = pos comp + 1 + numParams op
      computer' = comp { pos = pos' }
  in case op of
  Halt -> computer' { halt = True }
  Binop f -> memWrite (last values) (foldl1 f resolved) computer'

readInputFileOrIntcode :: IO [Int]
readInputFileOrIntcode = do
  args <- getArgs
  case args of
    [] -> fmap (map read . splitOn ',') (readFile "/dev/stdin")
    a:_ | ',' `elem` a -> return (map read (splitOn ',' a))
    a:_ -> fmap (map read . splitOn ',') (readFile a)

splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'
