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

append :: List a -> List a -> List a
append (Cons car1 cdr1) l2 = Cons car1 (append cdr1 l2)
append Nil (Value x) = Cons (Value x) Nil
append Nil list = list

accSeq :: (Show a) => (a -> b -> b) -> b -> List a -> b
accSeq op init Nil = init
accSeq op init (Cons (Value car) cdr) = op car (accSeq op init cdr)
accSeq op init (Cons carList cdr)     = accSeq op (accSeq op init carList) cdr

filtSeq :: (Show a) => (a -> Bool) -> List a -> List a
filtSeq f Nil = Nil
filtSeq f (Cons (Value car) cdr) | f car = Cons (Value car) (filtSeq f cdr)
                                 | otherwise   = filtSeq f cdr

uniquePairs :: (Show a, Num a, Enum a) => a -> List (List a)
uniquePairs n = accSeq append Nil (fmap f (list [1..n]))
  where
    f = \i -> fmap (\j -> list [i,j]) (list [1..(i-1)])

sumPairs :: (Show a, Num a, Enum a, Eq a) => a -> a -> List (List a)
sumPairs n s = filtSeq sumEq (uniquePairs n)
  where
    sumEq pair = (car pair) + (car (cdr pair)) == s

main :: IO ()
main = do
  pairs <- return $ sumPairs 4 4
  putStrLn $ show pairs

