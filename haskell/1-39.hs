main :: IO ()
main = do
  putStrLn $ show $ (tanCf 1.0 10)

contFrac :: (Eq a, Num a, Fractional b) => (a -> b) -> (a -> b) -> a -> b
contFrac fN fD k = recurse 1
  where recurse i = if i == k
                      then fN k / fD k
                      else fN i / (fD i + recurse (i + 1))

tanCf :: Double -> Int -> Double
tanCf x k = contFrac (indexN x) indexD k

indexN :: Double -> Int -> Double
indexN x k = if k == 1
                then x
                else -x^2

indexD :: Int -> Double
indexD k = fromIntegral . last $ take k $ filter odd [1..]
