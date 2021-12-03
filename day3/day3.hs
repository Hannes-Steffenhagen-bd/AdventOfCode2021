{-# LANGUAGE RecordWildCards #-}

import qualified Data.List as List

main :: IO ()
main = interact $
  (++"\n") . show . solution . lines


-- Basically, just sum together the 1s and 0s in each column and check if the
-- sum exceeds half the number of lines, if yes the digit is 1 else 0 for
-- gamma, and epsilon is just gamma inverted

data Rates = Rates
  { epsilon :: Int
  , gamma   :: Int
  }

solution
  = multiplyRates
  . evalRates
  . List.foldl' (\(ys, l) xs -> (zipWith (+) xs ys, l+1)) (repeat 0, 0)
  . map (map toInt)

toInt '0' = 0
toInt '1' = 1

evalRates (sums, lines) = Rates
  { gamma = binaryToInt $ sumsAsBinary,
    epsilon = binaryToInt . map not $ sumsAsBinary
  } where
  sumsAsBinary = map (\s -> s > lines `div` 2) sums

binaryToInt = List.foldl' (\num digit -> num*2 + binaryDigitToInt digit) 0

binaryDigitToInt True  = 1
binaryDigitToInt False = 0

multiplyRates Rates{..} = epsilon * gamma
