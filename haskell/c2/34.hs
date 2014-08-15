data List a = Nil | Value a | Cons (List a) (List a)

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

hornerEval :: (Show a, Num a) => a -> List a -> a
hornerEval x coefficients = accSeq horner 0 coefficients
  where
    horner coefficient higherTerms = coefficient + x * higherTerms

main :: IO ()
main = do
  coefficients <- return $ list [1, 3, 0, 5, 0, 1]
  putStrLn $ show $ hornerEval 2 coefficients
