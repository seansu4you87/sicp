data List a = ListEmpty | ListPair (a, List a) deriving (Show, Eq)

car :: List a -> a
car ListEmpty         = error "Empty List has no car!"
car (ListPair (a, _)) = a

cdr :: List a -> List a
cdr ListEmpty         = error "Empty List has no cdr!"
cdr (ListPair (_, l)) = l

mapList :: (a -> b) -> List a -> List b
mapList f ListEmpty = ListEmpty
mapList f (ListPair (x, xs)) = ListPair (f x, mapList f xs)

squareList :: (Num a) => List a -> List a
squareList ListEmpty = ListEmpty
squareList (ListPair (x, xs)) = ListPair (x^2, squareList xs)

squareList' :: (Num a) => List a -> List a
squareList' = mapList (\x -> x^2)

main :: IO ()
main = do
  nums <- return (ListPair (1, ListPair (2, ListPair (3, (ListPair (4, ListPair (5, ListEmpty)))))))
  putStrLn $ show $ squareList nums
  putStrLn $ show $ squareList' nums
