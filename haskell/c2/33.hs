data List a = Nil | Value a | Cons (List a) (List a) -- deriving (Show)

instance (Show a) => Show (List a) where
  show = ('(' :) . showBody
    where
      showBody Nil = ")"
      showBody (Value x)      = show x
      showBody (Cons car Nil) = showBody car ++ showBody Nil
      showBody (Cons car cdr) = showBody car ++ " " ++ showBody cdr

list :: (Show a) => [a] -> List a
list []     = Nil
list (x:xs) = Cons (Value x) (list xs)

cons :: (Show a) => a -> List a -> List a
cons car cdr = Cons (Value car) cdr

accSeq :: (Show a) => (a -> b -> b) -> b -> List a -> b
accSeq op init Nil = init
accSeq op init (Cons (Value car) cdr) = op car (accSeq op init cdr)

mapSeq :: (Show a, Show b) => (a -> b) -> List a -> List b
mapSeq proc = accSeq mapCons Nil
  where mapCons = \car acc -> cons (proc car) acc

appendSeq :: (Show a) => List a -> List a -> List a
appendSeq seq1 seq2 = accSeq cons seq2 seq1

lengthSeq :: (Show a) => List a -> Integer
lengthSeq seq = accSeq count 0 seq
  where count = \car acc -> acc + 1

main :: IO ()
main = do
  seq <- return (list [1,2,3,4,5])

  putStrLn $ show $ accSeq cons Nil seq
  putStrLn $ show $ mapSeq (\x -> x^2) seq
  putStrLn $ show $ appendSeq seq (list [6,7,8])
  putStrLn $ show $ lengthSeq seq
  putStrLn $ show $ lengthSeq (Cons (Value 1) Nil)
  -- putStrLn $ show $ lengthSeq Nil -- why the fuck doesn't this work?
