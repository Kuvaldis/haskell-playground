import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken"

lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD39I"))
        ,(101,(Free,"JAH3I"))
        ,(103,(Free,"IQSA9"))
        ,(105,(Free,"QOTSA"))
        ,(109,(Taken,"893JJ"))
        ,(110,(Taken,"99292"))
        ]


data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Yellow == Yellow = True
    Green == Green = True
    _ == _ = False
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- yes no
class YesNo a where
    yesno :: a -> Bool
instance YesNo Int where
    yesno 0 = False
    yesno _ = True
instance YesNo [a] where
    yesno [] = False -- 'yesno ""' == False
    yesno _ = True
-- if Maybe contains anything, then it is True
instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False
-- if a traffic lite is Red, then it is false
instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
-- 'if' function for yesno values
yesnoif :: (YesNo y) => y -> a -> a -> a
yesnoif yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- functor
-- The definition is like this
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b
-- 'f' here is not a concrete type, like Int, Bool or Maybe String, but a type constructor that takes one type parameter
-- for instance, map is just a fmap that works only on lists.
instance Functor' [] where
    fmap' = map
-- map is (a -> b) -> [a] -> [b], which is in fact means that [a] is a concrete typ, i.e. list of some values,
-- while [] is a typeclass, which corresponds to 'f' in definition of Functor.
instance Functor' Maybe where
    fmap' fn (Just x) = Just (fn x) -- for any function and Maybe with present value, i.e. (Just x) return Just containing value (fn x)
    fmap' fn Nothing = Nothing
instance Functor' (Either a) where
    -- here fmap is: (b -> c) -> Either a b -> Either a c
    fmap' fn (Right x) = Right (fn x)
    fmap' fn (Left x) = Left x
instance Ord k => Functor' (Map.Map k) where
    fmap' fn m = Map.fromList (map modify (Map.toList m))
        where modify (k, v) = (k, fn v)