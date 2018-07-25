module Analysis.WordsCount where

import Control.Exception (catch, SomeException)
import System.Environment (getArgs)

main = countWordsInFile

countWordsInFile :: IO ()
countWordsInFile = do
  args <- getArgs
  let filename =
        case args of
          (a:_) -> a
          _ -> "input.txt"
  input <- catch (readFile filename)
--   >> means perform first action (print) then second (return "", i.e. wrapping empty string into IO)
             (\err -> print (err::SomeException) >> return "")
  print $ countWords input

-- splits text to lines, for each line number of words is calculated
countWords :: String -> [Int]
countWords input = map (length . words) (lines input)

-- next 15