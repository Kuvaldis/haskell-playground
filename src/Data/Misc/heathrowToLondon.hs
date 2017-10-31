import Data.List
-- (A)--50--(A1)--5---(A2)--40--(A3)--10--(A4)
--           |         |         |         |
--           30        20        25        0
--           |         |         |         |
-- (B)--10--(B1)--90--(B2)--2---(B3)--8---(B4)

-- each part of this can be represented as a section consisting of 3 paths:
-- between A-points (path A), between B-points (path B) and between A and B (path C).
-- for instance, first section is 50, 10, 30; second is 5, 90, 20 etc.
data Section = Section { getA :: Int
                       , getB :: Int
                       , getC :: Int
                       } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

-- represents what path type (label) was chosen: A, B or C
data Label = A | B | C deriving (Show)

-- to calculate the actual path
type Path = [(Label, Int)]

-- the result should be [(B, 10), (C, 30), (A, 5), (C, 20), (B, 2), (B, 8)]

-- for each section we want to calculate the best path to A and B points
-- for this we need to know the previous step best paths
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let priceA = sum $ map snd pathA -- sums all path prices from the beginning to the current A point
        priceB = sum $ map snd pathB -- same for B
        forwardPriceToA = priceA + a
        crossPriceToA   = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB   = priceA + a + c
        newPathToA = if forwardPriceToA <= crossPriceToA
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        newPathToB = if forwardPriceToB <= crossPriceToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
    in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
        bestPath = if sum (map snd bestAPath) <= sum (map snd bestBPath)
                        then reverse bestAPath
                        else reverse bestBPath
        in bestPath

-- splits a list of values on list of specified length lists
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
    contents <- getContents
    let triplets = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) triplets
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathPrice = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice