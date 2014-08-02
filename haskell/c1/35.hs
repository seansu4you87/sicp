main :: IO ()
main = do
  putStrLn $ show phi

tolerance = 0.00001

fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f guess = if closeEnough guess (f guess)
                       then f guess
                       else fixedPoint f (f guess)

closeEnough :: Double -> Double -> Bool
closeEnough v1 v2 = abs (v1 - v2) < tolerance

fPhi :: Double -> Double
fPhi x = 1 + 1/x

phi :: Double
phi = fixedPoint fPhi 1.0
