data Interval a = Interval a a deriving (Show)

makeInterval :: a -> a -> Interval a
makeInterval l u = Interval l u

lowerBound :: Interval a -> a
lowerBound (Interval l u) = l

upperBound :: Interval a -> a
upperBound (Interval l u) = u

addInterval :: Num a => Interval a -> Interval a -> Interval a
addInterval x y = makeInterval (lowerBound x + lowerBound y) (upperBound x + upperBound y)

subInterval :: Num a => Interval a -> Interval a -> Interval a
subInterval x y = addInterval x (makeInterval (-1 * upperBound y) (-1 * lowerBound x))

mulInterval :: (Ord a, Num a) => Interval a -> Interval a -> Interval a
mulInterval x y = makeInterval (minimum [p1,p2,p3,p4]) (maximum [p1,p2,p3,p4])
  where p1 = lowerBound x * lowerBound y
        p2 = lowerBound x * upperBound y
        p3 = upperBound x * lowerBound y
        p4 = upperBound x * upperBound y

divInterval :: (Ord a, Fractional a) => Interval a -> Interval a -> Interval a
divInterval x y | lowerBound y * upperBound y < 0 = error "Division error (interval spans 0)"
                | otherwise = mulInterval x (makeInterval (1.0/upperBound y) (1.0/lowerBound y))

widthInterval :: Fractional a => Interval a -> a
widthInterval i = (lowerBound i + upperBound i) / 2

main :: IO ()
main = do
  x <- return $ makeInterval 1.0 2.0
  y <- return $ makeInterval (-1.0) 1.0
  putStrLn $ show $ (divInterval x y)
