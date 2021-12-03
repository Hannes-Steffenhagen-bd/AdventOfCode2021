{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = interact $
  (++"\n") . show . fst . foldl increment (0, Nothing) . map (read @Int) . lines

increment (incCount, lastMeasure) currentMeasure
  | currentMeasure `greaterThan` lastMeasure = (incCount + 1, Just currentMeasure)
  | otherwise = (incCount, Just currentMeasure)

greaterThan x Nothing = False
greaterThan x (Just y) = x > y
