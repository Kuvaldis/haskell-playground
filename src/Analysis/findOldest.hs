import Text.CSV

main :: IO ()
main = do
  let filename = "input.csv"
  input <- readFile filename
  let csv = parseCSV filename input
  either handleError doWork csv

handleError csv = putStrLn "error parsing"
doWork = print . findOldest . tail

findOldest :: [Record] -> Record
findOldest [] = []
findOldest xs = foldl1 (\a x -> if age x > age a then x else a) xs

age [a, b] = toInt b

toInt :: String -> Int
toInt = read