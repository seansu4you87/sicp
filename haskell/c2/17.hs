data List a = ListEmpty | ListPair (a, List a) deriving (Show, Eq)

car :: List a -> a
car ListEmpty         = error "Empty List has no car!"
car (ListPair (a, _)) = a

cdr :: List a -> List a
cdr ListEmpty         = error "Empty List has no cdr!"
cdr (ListPair (_, l)) = l

lastPair :: List a -> List a
lastPair ListEmpty                 = error "Empty List has no last pair!"
lastPair (ListPair (x, ListEmpty)) = ListPair (x, ListEmpty)
lastPair (ListPair (_, pair))      = lastPair pair

main :: IO ()
main = do
  list <- return (ListPair (23, ListPair (72, ListPair (149, ListPair (34, ListEmpty)))))
  putStrLn $ show $ lastPair list
