type Coin = Double
type Amount = Double

countChange :: Amount -> [Coin] -> Int
countChange amount coins
  | amount == 0 = 1
  | amount < 0 || length coins == 0 = 0
  | otherwise = (countChange (amount - head coins) coins) + (countChange amount (tail coins))

main :: IO ()
main = do
  usCoins <- return [50,25,10,5,1]
  putStrLn $ show $ countChange 100 usCoins
