import Debug.Trace

main :: IO ()
main = do
  putStrLn $ show $ fixedPoint (\x -> log 1000 / log x) 1.1

tolerance = 0.00001

fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f guess | trace (show guess) False = undefined
fixedPoint f guess = if closeEnough guess (f guess)
                       then f guess
                       else fixedPoint f (f guess)

closeEnough :: Double -> Double -> Bool
closeEnough v1 v2 = abs (v1 - v2) < tolerance
