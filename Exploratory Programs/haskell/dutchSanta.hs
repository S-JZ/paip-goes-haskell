
import Data.Map
import Data.Maybe
import Data.List

directions = ["north", "northeast", "east", "southeast", "south", "southwest", "west", "northwest"]

data Instruction = Instruction {command :: String, strides :: Int} deriving (Show)

fetchInstruction :: String -> Instruction 
fetchInstruction s = Instruction command (read strides :: Int) where (command : strides : []) = words s

directionToCoordinate = fromList [("north", (0, 1)), ("northeast", (1, 1)), ("east", (1, 0)), ("southeast", (1, -1)), ("south", (0, -1)), ("southwest", (-1, -1)), ("west", (-1, 0)), ("northwest", (-1, 1))]

f (x, y) z = (x * z, y * z)

add (x, y) (x', y') = (x + x', y + y')

jump :: Int -> String -> [(Int, Int)] -> ([(Int, Int)], String)
jump steps direction coords = (coords ++ [add (last coords) (f c steps)], direction)
                             where c = directionToCoordinate ! direction

move :: Int -> String -> [(Int, Int)] -> ([(Int, Int)], String)
move steps direction coords =(coords ++ (Prelude.map (add (last coords)) (Prelude.map (f c) [1..steps])), direction) 
                             where c = directionToCoordinate ! direction

spin :: Int -> String -> [(Int, Int)] -> ([(Int, Int)], String)
spin degrees direction coords =  (coords, directions !! (quot next 45))
                                 where curr = 45 * (fromMaybe (0) $ elemIndex direction directions)
                                       next = if curr + degrees < 0 
                                              then 360 + (curr + degrees)
                                              else rem (curr + degrees) 360
                                    

moveSanta :: String -> [(Int, Int)] -> String -> Int -> ([(Int, Int)], String)
moveSanta direction coords "draai" degrees  = spin degrees direction coords
moveSanta direction coords "loop" strides   = move strides direction coords
moveSanta direction coords "spring" strides = jump strides direction coords


apply :: String -> [(Int, Int)] -> [Instruction] -> [(Int, Int)]
apply d c []     = c
apply d c (x:xs) = apply d' c' xs
                  where (c', d') = moveSanta d c (command x) (strides x)

manhattanDistance xs = abs(fst (last xs)) + abs(snd (last xs))


main = do
  contents <- readFile "dutch.txt"
  let fileLines =  lines contents
  print $ fileLines
  let instructions = Prelude.map fetchInstruction fileLines
  print $ instructions 
  let coordinates = apply "north" [(0,0)] instructions
  print $ coordinates 
  print $ manhattanDistance coordinates

