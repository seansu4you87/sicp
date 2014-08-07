data List a = ListEmpty | ListPair (a, List a) deriving (Show, Eq)

car :: List a -> a
car ListEmpty         = error "Empty List has no car!"
car (ListPair (a, _)) = a

cdr :: List a -> List a
cdr ListEmpty         = error "Empty List has no cdr!"
cdr (ListPair (_, l)) = l

reverseList :: List a -> List a
reverseList ListEmpty = error "Empty List can't be reversed"
reverseList list = iter list ListEmpty
  where iter ListEmpty acc = acc
        iter list acc      = iter (cdr list) (ListPair ((car list), acc))

main :: IO ()
main = do
  list <- return (ListPair (1, ListPair (4, ListPair (9, ListPair (16, ListPair (25, ListEmpty))))))
  putStrLn $ show $ reverseList list
