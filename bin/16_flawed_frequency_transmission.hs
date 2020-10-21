import AdventOfCode (readInputFile)

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array.MArray (getElems, modifyArray, newArray)
import Data.Array.ST (STUArray)
import Data.Array.Unboxed ((!), UArray, bounds, elems, indices, listArray)
import Data.Bits ((.&.))
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

binom99Mod10 :: Int -> Int
binom99Mod10 i = (b2 * 5 + b5 * 6) `mod` 10
  where b2 = if (i + 99) .&. 99 == 99 then 1 else 0
        b5 = case i `mod` 125 of
             0 -> 1
             25 -> 4
             _ -> 0

fft2 :: UArray Int Int -> Int -> [Int]
fft2 digits offset = runST $ do
  let (_, len0) = second succ (bounds digits)
      len10k = len0 * 10000 - offset
  a <- newArray (0, 7) 0 :: ST s (STUArray s Int Int)
  for_ [0 .. (len10k - 1)] $ \i -> do
    let distFromEnd = len10k - i
        n = min 8 distFromEnd
        bin = binom99Mod10 i
    when (bin /= 0) $
      for_ [0 .. n - 1] $ \j -> do
        let idx = (offset + i + j) `mod` len0
        modifyArray a j (+ (bin * digits ! idx))
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
