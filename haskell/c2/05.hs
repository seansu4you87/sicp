cons :: Integer -> Integer -> Integer
cons a b = 2^a * 3^b

car :: Integer -> Integer
car z | mod z 2 > 0 = 0
      | otherwise = 1 + car (div z 2)

cdr :: Integer -> Integer
cdr z | mod z 3 > 0 = 0
      | otherwise = 1 + cdr (div z 3)

main :: IO ()
main = do
  putStrLn $ show $ car $ cons 1 2
  putStrLn $ show $ cdr $ cons 1 2

  putStrLn $ show $ car $ cons 5 9
  putStrLn $ show $ cdr $ cons 5 9
