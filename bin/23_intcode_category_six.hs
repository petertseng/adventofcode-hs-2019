{-# LANGUAGE TupleSections #-}

import AdventOfCode.Intcode (Computer(output), computer, continueInput, continueInputs, readInputFileOrIntcode)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (foldl')

type Packet = (Int, Int)

data Queue a = Queue [a] [a]

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _ = False

push :: a -> Queue a -> Queue a
push e (Queue inb out) = Queue (e:inb) out

pop :: Queue a -> (Maybe a, Queue a)
pop q@(Queue [] []) = (Nothing, q)
pop (Queue inb []) = pop (Queue [] (reverse inb))
pop (Queue inb (x:xs)) = (Just x, Queue inb xs)

runNet :: IntMap Computer -> (Int, Int)
runNet computers = runNet' computers 0 0 0 (0, 0) (IntMap.fromList (map (, Queue [] []) [0 .. 49]))

runNet' :: IntMap Computer -> Int -> Int -> Int -> Packet -> IntMap (Queue Packet) -> (Int, Int)
runNet' comps n firstNatRecvY lastNatSentY lastNatRecv queues =
  let (input, queues') = case pop (queues IntMap.! n) of
        (Nothing, _) -> ([-1], queues)
        (Just (x, y), q') -> ([x, y], IntMap.insert n q' queues)
      comp' = continueInputs input ((comps IntMap.! n) { output = [] })
      comps' = IntMap.insert n comp' comps
      (queues'', lastNatRecv') = foldl' doOutput (queues', lastNatRecv) (trips (output comp'))
      firstNatRecvY' = if lastNatRecv' /= (0, 0) && firstNatRecvY == 0 then snd lastNatRecv' else firstNatRecvY
      n' = if n == 49 then 0 else n + 1
  in if n == 49 && all empty queues'' then
       if lastNatSentY == snd lastNatRecv then (firstNatRecvY, lastNatSentY)
       else runNet' comps' n' firstNatRecvY' (snd lastNatRecv') lastNatRecv' (IntMap.insert 0 (Queue [] [lastNatRecv']) queues'')
     else runNet' comps' n' firstNatRecvY' lastNatSentY lastNatRecv' queues''

trips :: [a] -> [(a, a, a)]
trips (x:y:z:xs) = (x, y, z) : trips xs
trips [] = []
trips l = error ("expected triples not " ++ show (length l))

doOutput :: (IntMap (Queue Packet), Packet) -> (Int, Int, Int) -> (IntMap (Queue Packet), Packet)
doOutput (queues, _) (255, x, y) = (queues, (x, y))
doOutput (queues, lastNat) (addr, x, y) =
  let queue = push (x, y) (queues IntMap.! addr)
  in (IntMap.insert addr queue queues, lastNat)

main :: IO ()
main = do
  mem <- readInputFileOrIntcode

  let comps = IntMap.fromList (map (\i -> (i, continueInput i (computer mem))) [0 .. 49])
  let (firstY, lastY) = runNet comps
  print firstY
  print lastY
