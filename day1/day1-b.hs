{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = interact $
  (++ "\n") . show . solution . map (read @Int) . lines

solution = fst . foldl increment (0, Nothing) . windows

windows measurements = zipWith (+) measurements $
  zipWith (+)
          (tail measurements)
          (tail $ tail measurements)

increment (incCount, lastMeasure) currentMeasure
  | currentMeasure `greaterThan` lastMeasure = (incCount + 1, Just currentMeasure)
  | otherwise = (incCount, Just currentMeasure)

greaterThan x Nothing = False
greaterThan x (Just y) = x > y
