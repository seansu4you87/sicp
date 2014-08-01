repeated :: (a -> a) -> Int -> (a -> a)
repeated f n | n == 1 = f
             | otherwise = f . (repeated f (n-1))

type MathFunction = (Double -> Double)

smooth :: MathFunction -> MathFunction
smooth f = \x -> (f (x - dx) + f x + f (x + dx)) / 3
  where dx = 0.00001

nFoldSmooth :: Int -> MathFunction -> MathFunction
nFoldSmooth = repeated smooth
