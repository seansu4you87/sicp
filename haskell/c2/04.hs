cons :: a -> b -> (a -> b -> c) -> c
cons x y = \f -> f x y

car :: ((a -> b -> a) -> a) -> a
car z = z (\p q -> p)

cdr :: ((a -> b -> b) -> b) -> b
cdr z = z (\p q -> q)

main :: IO ()
main = do
  z <- return $ cons 1 2
  putStrLn $ show $ car z
  putStrLn $ show $ cdr z
