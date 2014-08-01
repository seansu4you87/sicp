double :: (a -> a) -> (a -> a)
double f = f . f

incr :: Int -> Int
incr = (+) 1
