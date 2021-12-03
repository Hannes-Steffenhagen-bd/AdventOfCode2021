main :: IO ()
main = interact $
  (++"\n") . show . solution . lines

solution = eval . foldl (flip applyCommand) (Position 0 0) . map (parseCommand . words)

data Command
  = Forward Int
  | Down Int
  | Up Int

parseCommand :: [String] -> Command
parseCommand ["forward", x] = Forward $ read x
parseCommand ["down", x]    = Down $ read x
parseCommand ["up", x]      = Up $ read x

data Position = Position !Int !Int

applyCommand :: Command -> Position -> Position
applyCommand (Forward a) (Position x y) = Position (x+a) y
applyCommand (Down a) (Position x y)    = Position x (y+a)
applyCommand (Up a) (Position x y)      = Position x (y-a)

eval :: Position -> Int
eval (Position x y) = x*y
