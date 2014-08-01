import Debug.Trace

type Function = (Double -> Double)

fixedPoint :: Function -> Double -> Double
fixedPoint f guess | trace (show guess) False = undefined
fixedPoint f guess = if closeEnough guess (f guess)
                       then f guess
                       else fixedPoint f (f guess)

closeEnough :: Double -> Double -> Bool
closeEnough v1 v2 = abs (v1 - v2) < tolerance
  where tolerance = 0.00001

deriv :: Function -> Function
deriv g = \x -> (g (x + dx) - g x) / dx
  where dx = 0.00001

newtonTransform :: Function -> Function
newtonTransform g = \x -> x - (g x / ((deriv g) x))

newtonsMethod :: Function -> Double -> Double
newtonsMethod g guess = fixedPoint (newtonTransform g) guess

cubic :: Double -> Double -> Double -> Function
cubic a b c = \x -> x^3 + a*x^2 + b*x + c

main :: IO ()
main = do
  putStrLn $ show (newtonsMethod (cubic 3 3 1) 1)
