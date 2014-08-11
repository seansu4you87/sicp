import Debug.Trace

data List a = Nil | Value a | Cons (List a) (List a) deriving (Show)

-- instance (Show a) => Show (List a) where
--   show = ('(' :) . go
--     where
--       go Nil = ")"
--       go (Value x) = show x
--       go (Cons car cdr) = show car ++ " " ++ go cdr

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Value x) = Value (f x)
  fmap f (Cons car cdr) = Cons (fmap f car) (fmap f cdr)

append :: List a -> List a -> List a
append (Cons car1 cdr1) l2 = Cons car1 (append cdr1 l2)
-- append Nil Nil = error "cannot append Nil to Nil"
append Nil (Value x) = Cons (Value x) Nil
append Nil list = list

subsets :: (Show a) => List a -> List (List a)
subsets Nil = Nil
subsets (Value x) = (Cons (Value (Cons (Value x) Nil)) Nil)
subsets (Cons car cdr) = append tailSubsets (appendToSets tailSubsets car)
  where tailSubsets             = subsets cdr
        appendToSets Nil value  = Cons emptySet (Cons (setWithValue value) Nil)
        appendToSets sets value = fmap (\set -> append set value) sets
        setWithValue (Value x)  = Value (Cons (Value x) Nil)
        emptySet                = Value Nil

main :: IO ()
main = do
  set <- return (Cons (Value 1) Nil)
  putStrLn $ show set

  set <- return $ append set (Value 2)
  putStrLn $ show set

  set <- return $ append set (Value 3)
  putStrLn $ show set

--   set <- return $ append set (Cons (Value 4) (Cons (Value 5) Nil))
--   putStrLn $ show set

  putStrLn $ show $ subsets set
