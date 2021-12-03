{-# LANGUAGE TypeApplications #-}

main :: IO ()
main = interact $
  (++ "\n") . show . solution . map (read @Int) . lines

solution = countIncrements . windows

windows measurements = zipWith (+) measurements $
  zipWith (+)
          (tail measurements)
          (tail $ tail measurements)

countIncrements ws = length . filter id $
  zipWith (<)
          ws
          (tail ws)
