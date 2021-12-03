{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.List as List
import Text.Printf

main :: IO ()
main = interact $
  (++"\n") . show . solution . lines


-- we'll just reuse the binary strings we calculate for epsilon/gamma, and then
-- filter based on those

data Rates = Rates
  { oxygenGenerator :: Int
  , carbonDioxideScrubber   :: Int
  }

solution lines
  = multiplyRates
  $ findGeneratorAndScrubberRates readingsAsBool
  where readingsAsBool = map (map toBool) lines

multiplyRates Rates{..} = oxygenGenerator * carbonDioxideScrubber

findGeneratorAndScrubberRates readings
  = Rates {..}
  where
    oxygenGenerator = binaryToInt $ filterReadingBy id readings
    carbonDioxideScrubber = binaryToInt $ filterReadingBy not readings

filterReadingBy f readings = go (zip readings readings) where
  go [(reading, _)] = reading
  go readings =
    let
      (d:_)
        = map f
        . mostCommonDigits
        $ map snd readings
    in
      go
    . map (\(r, rds) -> (r, tail rds))
    . filter (\(_, rd:_) -> d == rd)
    $ readings

toBool '0' = False
toBool '1' = True

mostCommonDigits :: [[Bool]] -> [Bool]
mostCommonDigits ds = let cutoff = ceiling @Double (fromIntegral (length ds) / 2) in 
    map (\s -> s >= cutoff)
  . List.foldl' (zipWith (+)) (repeat 0)
  $ map (map binaryDigitToInt) ds

binaryToInt = List.foldl' (\num digit -> num*2 + binaryDigitToInt digit) 0

binaryDigitToInt True  = 1
binaryDigitToInt False = 0
