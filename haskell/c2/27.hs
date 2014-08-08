data List a = ListEmpty | ListPair (a, List a) deriving (Show, Eq)

car :: List a -> a
car ListEmpty         = error "Empty List has no car!"
car (ListPair (a, _)) = a

cdr :: List a -> List a
cdr ListEmpty         = error "Empty List has no cdr!"
cdr (ListPair (_, l)) = l

makeList :: [a] -> List a
makeList [] = ListEmpty
makeList (x:xs) = ListPair (x, makeList xs)

-- I don't think you can actually do this genericly?  The Type systems is bugging me out!
isListPair :: a -> Bool
isListPair (ListPair (_,_)) = True
isListPair _ = False

reverseList :: List a -> List a
reverseList ListEmpty = error "Empty List can't be reversed"
reverseList list = iter list ListEmpty
  where iter ListEmpty acc = acc
        iter list      acc = iter (cdr list) (ListPair ((car list), acc))

-- deepReverseList :: List a -> List a
-- deepReverseList ListEmpty = error "Empty List can't be reversed"

main :: IO ()
main = do
  x <- return $ makeList [(makeList [1,2]), (makeList [3,4])]
  putStrLn $ show x
  putStrLn $ show $ reverseList x
  -- putStrLn $ show $ deepReverseList x

