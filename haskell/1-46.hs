import Debug.Trace

type MathFunction = (Double -> Double)
type Checker = (Double -> Bool)
type Improver = MathFunction

iterImprove :: Checker -> Improver -> MathFunction
iterImprove check improve = \x -> iterate x
  where iterate guess | trace (show guess) False = undefined
                      | check guess = guess
                      | otherwise = iterate $ improve guess

fixedPoint :: MathFunction -> Double -> Double
fixedPoint f x = (iterImprove check f) x
  where check guess | abs (guess - f guess) < 0.00001 = True
                    | otherwise = False

sqrtFixed :: MathFunction
sqrtFixed x = fixedPoint (\y -> (y + x/y) / 2) 1.0

sqrtNewt :: MathFunction
sqrtNewt x = (iterImprove check improve) x
  where check guess | abs (guess*guess - x) < 0.00001 = True
                    | otherwise = False
        improve guess = (guess + x/guess) / 2

main :: IO ()
main = do
  putStrLn "Fixed Point"
  putStrLn $ show (sqrtFixed 4)

  putStrLn "\nNewton's Method"
  putStrLn $ show (sqrtNewt 4)
