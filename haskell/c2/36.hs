import Control.Applicative

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

accSeq :: (Show a) => (a -> b -> b) -> b -> List a -> b
accSeq op init Nil = init
accSeq op init (Cons (Value car) cdr) = op car (accSeq op init cdr)
accSeq op init (Cons carList cdr)     = accSeq op (accSeq op init carList) cdr

accSeqN :: (Show a, Eq a, Show b) => (a -> b -> b) -> b -> List (List a) -> List b
accSeqN op init seqs
                  | car seqs == Nil = Nil
                  | otherwise       = cons (accSeq op init cars) (accSeqN op init cdrs)
                  where
                    cars = fmap car seqs
                    cdrs = fmap cdr seqs

main :: IO ()
main = do
  seq1 <- return $ list [1, 2, 3]
  seq2 <- return $ list [4, 5, 6]
  seq3 <- return $ list [7, 8, 9]
  seq4 <- return $ list [10, 11, 12]
  seqs <- return $ list [seq1, seq2, seq3, seq4]
  putStrLn $ show $ accSeqN (+) 0 seqs
