main :: IO ()
main = do
  putStrLn $ show $ contFrac (\k -> 1.0)
                             (\k -> 1.0)
                             11

contFrac :: (Float -> Float) -> (Float -> Float) -> Float -> Float
contFrac fN fD k = if k == 0
                     then 0
                     else fN k / (fD k + contFrac fN fD (k-1))
