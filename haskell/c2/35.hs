import Control.Applicative

data List a = Nil | Value a | Cons (List a) (List a)

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

lenghList :: List a -> Int
lenghList Nil = 0
lenghList (Cons _ cdr) = 1 + lenghList cdr

cons :: (Show a) => a -> List a -> List a
cons car cdr = Cons (Value car) cdr

accSeq :: (Show a) => (a -> b -> b) -> b -> List a -> b
accSeq op init Nil = init
accSeq op init (Cons (Value car) cdr) = op car (accSeq op init cdr)
accSeq op init (Cons carList cdr)     = accSeq op (accSeq op init carList) cdr

mapSeq :: (Show a) => (a -> b) -> List a -> List b
mapSeq f seq = accSeq mapCons Nil seq
  where
    mapCons car cdr = Cons (Value (f car)) cdr

countLeaves :: (Show a) => List a -> Int
countLeaves tree = accSeq (+) 0 leafCounts
  where
    leafCounts = idLeaf <$> tree
    idLeaf = \_ -> 1

main :: IO ()
main = do
  tree <- return $ Cons (Cons (Value 1)
                              (Cons (Value 2) Nil))
                        (Cons (Value 3)
                              (Cons (Value 4) Nil))
  putStrLn $ show $ countLeaves tree
