data Rat = Rat Integer Integer

instance Show Rat where
  show x = show (numer x) ++ "/" ++ show (denom x)

makeRat :: Integer -> Integer -> Rat
makeRat n d = Rat (div n g) (div d g)
  where g = sign * gcd n d
        sign | d < 0 = -1
             | otherwise = 1

numer :: Rat -> Integer
numer (Rat n _) = n

denom :: Rat -> Integer
denom (Rat _ d) = d

addRat :: Rat -> Rat -> Rat
addRat x y = makeRat (numer x * denom y + numer y * denom x) (denom x * denom y)

subRat :: Rat -> Rat -> Rat
subRat x y = makeRat (numer x * denom y - numer y * denom x) (denom x * denom y)

mulRat :: Rat -> Rat -> Rat
mulRat x y = makeRat (numer x * numer y) (denom x * denom y)

divRat :: Rat -> Rat -> Rat
divRat x y = makeRat (numer x * denom y) (numer y * denom x)

half = makeRat (-1) (-2)
third = makeRat 1 3

main :: IO ()
main = do
  putStrLn $ show half
  putStrLn $ show $ addRat half third
  putStrLn $ show $ mulRat half third
  putStrLn $ show $ addRat third third
