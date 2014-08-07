data List a = ListEmpty | ListPair (a, List a) deriving (Show, Eq)

car :: List a -> a
car ListEmpty         = error "Empty List has no car!"
car (ListPair (a, _)) = a

cdr :: List a -> List a
cdr ListEmpty         = error "Empty List has no cdr!"
cdr (ListPair (_, l)) = l

filterList :: (a -> Bool) -> List a -> List a
filterList f ListEmpty = ListEmpty
filterList f (ListPair (x, xs))
  | f x       = ListPair (x, filterList f xs)
  | otherwise = filterList f xs

sameParity :: (Integral a) => List a -> List a
sameParity ListEmpty            = ListEmpty
sameParity list = filterList (parity $ car list) list
  where parity num
          | even num  = even
          | otherwise = odd

main :: IO ()
main = do
  a <- return $ ListPair (1, ListPair (2, ListPair (3, ListPair (4, ListPair (5, ListPair (6, ListPair (7, ListEmpty)))))))
  b <- return $ ListPair (2, ListPair (3, ListPair (4, ListPair (5, ListPair (6, ListPair (7, ListEmpty))))))
  putStrLn $ show $ sameParity a
  putStrLn $ show $ sameParity b
