import Data.List
import qualified Data.Map as Map
import qualified Geometry.Cube as Cube
-- only nub and sort are imported
-- import Data.List (nub, sort)
-- import everything except nub
-- import Data.List hiding (nub)
-- qualified import. Means you have to write Data.Map.<function name> explicitly or some short name using 'as'.
-- Needed to prevent name clashes
-- import qualified Data.Map as M

-- num removes duplicates
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- put given element in between elements in the list
-- intersperse '.' "MONKEY" == "M.O.N.K.E.Y"
-- put given element in between elements in the list, compose and flatten
-- intercalate " " ["hey", "you", "there"] == "hey you there"
-- concats
-- concat ["foo","bar","car"] == "foobarcar"
-- maps and concats
-- concatMap (replicate 4) [1..3] == [1,1,1,1,2,2,2,2,3,3,3,3]
-- performs and for each element. there are also or, any, all,
-- and $ map (>4) [5,6,7,8] == False
-- iterates. gets 1, then 2, then 4 etc. infinitely
-- iterate (*2) 1

-- count uniques number
numUniquesWithValues :: (Ord a) => [a] -> [(a, Int)]
numUniquesWithValues xs = map (\l@(x:xs) -> (x, length l)) . group . sort $ xs
-- numUniquesWithValues [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7] == [(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    -- tails: "ABC" -> ["ABC", "BC", "C", ""]
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

data Maybe a = Nothing | Just a deriving (Show, Eq, Ord)
-- Nothing < Just 100 true
-- Just 100 > Just 50 true

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

showPerson = show Person {firstName = "Michael", lastName = "Diamond", age = 43}
readPerson = (read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person) == Person {firstName = "Michael", lastName = "Diamond", age = 43}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- Eq and Ord
dayCompare = Monday `compare` Wednesday == LT
-- Show
dayShow = show Wednesday == "Wednesday"
-- Read
dayRead = (read "Wednesday" :: Day) == Wednesday
-- Bounded
dayMinBound = (minBound :: Day) == Monday
dayMaxBound = (maxBound :: Day) == Sunday
-- Enum
daySuccessor = (succ Monday) == Tuesday
dayPredecessor = (pred Saturday) == Friday
