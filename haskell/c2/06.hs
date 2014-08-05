zero :: a -> (b -> b)
zero = \f -> (\x -> x)

addOne n = \f -> (\x -> f ((n f) x))

one :: (a -> b) -> a -> b
one = \f -> (\x -> f x)
