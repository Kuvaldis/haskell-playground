import System.Random
import Data.List

-- standard generator can be created as mkStdGen 10

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins generator =
    let (firstCoin, newGenerator) = random generator -- random takes generator, returns a pair of generated value and new generator
        (secondCoin, newGenerator') = random newGenerator
        (thirdCoin, newGenerator'') = random newGenerator'
    in  (firstCoin, secondCoin, thirdCoin)

-- randoms generates infinite list of randoms
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

-- e.g. "finiteRandoms 10 $ mkStdGen 101 :: ([Bool], StdGen)"
finiteRandoms :: (RandomGen g, Random a, Integral n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
    in  (value:restOfList, finalGen)

-- Generates random string take 10 $ randomRs ('a', 'z') (mkStdGen 5)

main = do
    gen <- getStdGen -- returns global random generator
    -- to get a new random generator use newStdGen
    let randomChars = randomRs ('a', 'z') gen
        (first20, rest) = splitAt 20 randomChars
        (second20, _) = splitAt 20 rest
    putStrLn first20
    putStr second20