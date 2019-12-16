import AdventOfCode (readInputFile)

import Control.Arrow (second)
import Data.Array.Unboxed ((!), UArray, bounds, elems, indices, listArray)
import Data.Bits ((.&.))
import Data.Char (digitToInt, intToDigit)
import Data.List (dropWhileEnd, transpose)

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

binom :: Int -> Int -> Int
binom _ 0 = 1
binom n k = n * binom (n - 1) (k - 1) `div` k

binomMod :: Int -> Int -> Int -> Int
binomMod _ 0 _ = 1
binomMod n k m | n < m = binom n k `mod` m
binomMod n k m = case binom (n `mod` m) (k `mod` m) `mod` m of
  0 -> 0
  r -> (r * binomMod (n `div` m) (k `div` m) m) `mod` m

binomMod10 :: Int -> Int -> Int
binomMod10 n k = (b2 * 5 + b5 * 6) `mod` 10
  where b2 = if n .&. k == k then 1 else 0
        b5 = binomMod n k 5

fft2 :: UArray Int Int -> Int -> [Int]
fft2 digits offset = map ((`mod` 10) . sum) (transpose digitSums)
  where (_, len0) = second succ (bounds digits)
        len10k = len0 * 10000 - offset
        digitSums = zipWith digitSum [0..] binomCoefficients
        binomCoefficients = [binomMod10 (99 + i) i | i <- [0 .. (len10k - 1)]]
        digitSum _ 0 = []
        digitSum i bin =
          let distFromEnd = len10k - i
              n = min 8 distFromEnd
          in [bin * digits ! ((offset + i + j) `mod` len0) | j <- [0 .. (n - 1)]]

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
