import Debug.Trace

type MathFunction = (Double -> Double)

fixedPoint :: MathFunction -> Double -> Double
fixedPoint f guess | trace (show guess) False = undefined
fixedPoint f guess = if closeEnough guess (f guess)
                       then f guess
                       else fixedPoint f (f guess)
  where closeEnough v1 v2 = abs (v1 - v2) < tolerance
        tolerance = 0.00001

averageDamp :: MathFunction -> MathFunction
averageDamp f = \x -> (x + f x) / 2

repeated :: (a -> a) -> Int -> (a -> a)
repeated f n | n == 1 = f
             | otherwise = f . (repeated f (n-1))

square :: MathFunction
square = \x -> x^2

nRoot :: Int -> Double -> Double
nRoot n x = fixedPoint finalDampedRoot 1.0
  where finalDampedRoot = finalDamp root
        finalDamp = repeated averageDamp dampCount
        dampCount = floor (log (fromIntegral n) / log 2)
        root = \y -> x / (y^(n-1))

main :: IO ()
main = do
  putStrLn $ show $ nRoot 16 65536
