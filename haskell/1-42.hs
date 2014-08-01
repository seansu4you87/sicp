compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f $ g x

square :: Double -> Double
square x = x * x

incr :: Double -> Double
incr x = x + 1

main :: IO ()
main = do
  putStrLn $ show $ compose square incr 6
