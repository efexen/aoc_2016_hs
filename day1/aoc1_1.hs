import System.IO
import Data.List.Split
import qualified Data.Text as T

main = do
  contents <- readFile "input.txt"

  let (cleaned:_) = splitOn "\n" contents
  let instructions = (parseInstructions . trimInstructions . splitOn ",") cleaned
  let (x, y, _) = traverseInstructions (0,0,'U') instructions
  let distance = abs x + abs y

  print distance

trimInstructions :: [String] -> [String]
trimInstructions instructions = [trim i | i <- instructions]

trim :: String -> String
trim text = T.unpack $ T.strip $ T.pack text

parseInstructions :: [String] -> [(Char, Int)]
parseInstructions instructions = [(direction, read number::Int) | (direction:number) <- instructions]

traverseInstructions :: (Int, Int, Char) -> [(Char, Int)] -> (Int, Int, Char)
traverseInstructions position [] = position
traverseInstructions position ((direction, amount):tail) = do
  let newpos = applyInstruction position direction amount
  traverseInstructions newpos tail

applyInstruction :: (Int, Int, Char) -> Char -> Int -> (Int, Int, Char)
applyInstruction (x, y, 'L') 'R' amount = (x, y + amount, 'U')
applyInstruction (x, y, 'R') 'L' amount = (x, y + amount, 'U')
applyInstruction (x, y, 'U') 'R' amount = (x + amount, y, 'R')
applyInstruction (x, y, 'D') 'L' amount = (x + amount, y, 'R')
applyInstruction (x, y, 'D') 'R' amount = (x - amount, y, 'L')
applyInstruction (x, y, 'U') 'L' amount = (x - amount, y, 'L')
applyInstruction (x, y, _) _ amount = (x, y - amount, 'D')
