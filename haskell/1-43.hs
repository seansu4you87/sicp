repeated :: (a -> a) -> Int -> (a -> a)
repeated f n | n == 1 = f
             | otherwise = f . (repeated f (n-1))

square :: Double -> Double
square x = x^2

main :: IO ()
main = do
  putStrLn $ show $ (repeated square 2) 5
