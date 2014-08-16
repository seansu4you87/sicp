data List a = Nil | Value a | Cons (List a) (List a) deriving (Eq)

instance (Show a) => Show (List a) where
  show = ('(' :) . showBody
    where
      showBody Nil = ")"
      showBody (Value x)      = show x
      showBody (Cons car Nil) = showBody car ++ showBody Nil
      showBody (Cons car cdr) = showBody car ++ " " ++ showBody cdr

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Value x) = Value (f x)
  fmap f (Cons car cdr) = Cons (fmap f car) (fmap f cdr)

list :: (Show a) => [a] -> List a
list []     = Nil
list (x:xs) = Cons (Value x) (list xs)

car :: List a -> a
car Nil = error "cannot car Nil"
car (Value _) = error "cannot car a Value"
car (Cons (Value x) _) = x

cdr :: List a -> List a
cdr Nil = error "cannot cdr Nil"
cdr (Value _) = error "cannot cdr a Value"
cdr (Cons _ snd) = snd

cons :: (Show a) => a -> List a -> List a
cons car cdr = Cons (Value car) cdr

foldRight :: (Show a) => (a -> b -> b) -> b -> List a -> b
foldRight _  init Nil       = init
foldRight op init (Value x) = op x init
foldRight op init seq       = op head foldTail
  where
    foldTail = foldRight op init tail -- (1 op (2 op (3 op (4 op init))))
    head = car seq
    tail = cdr seq

foldLeft :: (Show a) => (b -> a -> b) -> b -> List a -> b
foldLeft _  init Nil       = init
foldLeft op init (Value x) = op init x
foldLeft op init seq       = iter init seq
  where
    iter acc Nil  = acc
    iter acc rest = iter opResult tailRest
      where
        opResult = op acc headRest -- (((init op 1) op 2) op 3) op 4
        headRest = car rest
        tailRest = cdr rest

append :: List a -> List a -> List a
append Nil Nil = Nil
append Nil (Value x) = Cons (Value x) Nil
append Nil list = list
append (Value x) Nil = Cons (Value x) Nil
append (Cons fst snd) Nil = Cons fst snd
append (Cons fst snd) (Value x) = Cons fst (append snd (Value x))
append (Cons fst snd) list2 = append list1AndList2Head list2Tail
  where
    list1 = Cons fst snd
    list1AndList2Head = append list1 list2Head
    list2Head = Value (car list2)
    list2Tail = cdr list2

reverseRight :: (Show a) => List a -> List a
reverseRight seq = foldRight f Nil seq
  where
    f x acc = append acc (list [x])

reverseLeft :: (Show a) => List a -> List a
reverseLeft seq = foldLeft f Nil seq -- (List b -> a -> List b) -> List b -> List a -> List b
  where
    f acc x = Cons (Value x) acc

main :: IO ()
main = do
  l <- return $ list [1,2,3]
  l2 <- return $ append l (Value 4)
  l3 <- return $ append l2 (list [5, 6])
  putStrLn $ show $ foldRight (-) 0 l -- 1 - (2 - (3 - 0)) = 1 - (2 - 3) = 1 - (-1) = 2
  putStrLn $ show $ foldLeft  (-) 0 l -- ((0 - 3) - 2) -1  = (-3 - 2) - 1 = -6
  putStrLn $ show l2
  putStrLn $ show l3
  putStrLn $ show $ reverseRight l3
  putStrLn $ show $ reverseLeft l3
