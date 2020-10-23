{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)

import Control.Arrow (second)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (getElems, modifyArray, newArray)
import Data.Array.ST (STUArray)
import Data.Array.Unboxed ((!), UArray, bounds, elems, indices, listArray)
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (for_)
import Data.List (dropWhileEnd)

fft1 :: UArray Int Int -> UArray Int Int
fft1 digits = listArray (bounds digits) (map fft (indices digits))
  where (_, len) = bounds digits
        fft i = abs (sum (multed i)) `mod` 10
        multed = map (\(mult, l, r) -> mult * ((sumLeft ! min r (len + 1)) - (sumLeft ! l))) . mults
        mults = takeWhile ((<= len) . snd3) . multsFor
        sumLeft = sumLeftArr digits

multsFor :: Int -> [(Int, Int, Int)]
multsFor n = iterate (\(sign, l, r) -> (-sign, l + n * 2, r + n * 2)) (1, n, n * 2)

sumLeftArr :: UArray Int Int -> UArray Int Int
sumLeftArr arr = listArray (second succ (bounds arr)) (scanl (+) 0 (elems arr))

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

fft2 :: UArray Int Int -> Int -> [Int]
fft2 digits offset = runST $ do
  let (_, len0) = second succ (bounds digits)
      len10k = len0 * 10000 - offset
  a <- newArray (0, 7) 0 :: ST s (STUArray s Int Int)
  let stride bigStride littleStrides = do
        for_ [0, bigStride .. (len10k - 1)] $ \bigStrideBase -> do
          for_ littleStrides $ \(littleStride, coeff) -> do
            let i = bigStrideBase + littleStride
                distFromEnd = len10k - i
                n = min 8 distFromEnd
            for_ [0 .. n - 1] $ \j -> do
              let idx = (offset + i + j) `mod` len0
              modifyArray a j (+ (coeff * digits ! idx))
  stride 128 [(x, 5) | x <- [0, 4 .. 28]]
  stride 125 [(0, 6), (25, 4)]
  es <- getElems a
  return (map (`mod` 10) es)

main :: IO ()
main = do
  s <- readInputFile
  let trimmed = dropWhileEnd (== '\n') s
  let digits = map digitToInt trimmed

  let digits1001 = iterate fft1 (listArray (1, length trimmed) digits) !! 100
  putStrLn (map intToDigit (take 8 (elems digits1001)))

  let offset = read (take 7 trimmed)
  let digits1002 = fft2 (listArray (0, length trimmed - 1) digits) offset
  putStrLn (map intToDigit digits1002)
