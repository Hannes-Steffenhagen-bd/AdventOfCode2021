main :: IO ()
main = interact $
  (++"\n") . show . solution . lines

solution = eval . foldl (flip applyCommand) (Position 0 0 0) . map (parseCommand . words)

data Command
  = Forward Int
  | Down Int
  | Up Int

parseCommand :: [String] -> Command
parseCommand ["forward", x] = Forward $ read x
parseCommand ["down", x]    = Down $ read x
parseCommand ["up", x]      = Up $ read x

data Position = Position !Int !Int !Int

applyCommand :: Command -> Position -> Position
applyCommand (Forward a) (Position aim x y) = Position aim (x+a) (y + aim*a)
applyCommand (Down a) (Position aim x y)    = Position (aim+a) x y
applyCommand (Up a) (Position aim x y)      = Position (aim-a) x y

eval :: Position -> Int
eval (Position _ x y) = x*y
