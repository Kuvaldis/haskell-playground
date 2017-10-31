main = interact respondPalindromes'

respondPalindromes :: String -> String
respondPalindromes input =
    let allLines = lines input
        palindrome = map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") allLines
        result = unlines palindrome
    in  result

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

respondPalindromes' :: String -> String
respondPalindromes' = unlines . map (\xs ->
    if isPalindrome' xs then "palindrome" else "not a palindrome") . lines
    where   isPalindrome' xs = xs == reverse xs