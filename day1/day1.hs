{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = interact $
  (++"\n") . show . solution . map (read @Int) . lines

solution measurements = length . filter id $
  zipWith (<)
          measurements
          (tail measurements)
