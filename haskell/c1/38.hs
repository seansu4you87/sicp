main :: IO ()
main = do
  putStrLn $ show $ (2 + contFrac (\k -> 1)
                                  indexD
                                  11)

contFrac :: (RealFrac a) => (a -> a) -> (a -> a) -> a -> a
contFrac fN fD k = recurse 1
  where recurse i = if i == k
                      then fN k / fD k
                      else fN i / (fD i + recurse (i + 1))

indexD :: (RealFrac a) => a -> a
indexD 1 = 1
indexD k | mod ((ceiling k)-2) 3 == 0 = 2 + 2 * (k-2) / 3
         | otherwise = 1.0
