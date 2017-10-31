import Data.List

-- implementation based on refverse polish notation
solveRPN :: (Num a, Read a) => String -> a
-- split expression on words: "10 4 3 + 2 * -" -> ["10", "4", "3", "+", "2", "*", "-"]
-- then fold from left to right. The accumulator is a stack (list here) which stores result of calculation,
-- adding numbers to the head of the list
-- for instance, ["10"], ["4", "10"], ["3", "4", "10"], ["7", "10"] (plus), etc.
solveRPN = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (y * x):ys
            foldingFunction (x:y:ys) "+" = (y + x):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys -- y minus x because first element in the stack is the latest, i.e. x
            foldingFunction xs numberString = (read numberString):xs


-- implementation based on refverse polish notation
solveRPN' :: String -> Float
-- split expression on words: "10 4 3 + 2 * -" -> ["10", "4", "3", "+", "2", "*", "-"]
-- then fold from left to right. The accumulator is a stack (list here) which stores result of calculation,
-- adding numbers to the head of the list
-- for instance, ["10"], ["4", "10"], ["3", "4", "10"], ["7", "10"] (plus), etc.
solveRPN' = head . foldl foldingFunction [] . words
    where   foldingFunction (x:y:ys) "*" = (y * x):ys
            foldingFunction (x:y:ys) "+" = (y + x):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys -- y minus x because first element in the stack is the latest, i.e. x
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction (x:xs) "ln"  = (log x):xs
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = (read numberString):xs