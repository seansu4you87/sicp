data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

squareTree :: (Num a) => Tree a -> Tree a
squareTree (Leaf x) = Leaf (x^2)
squareTree (Branch left right) = Branch (squareTree left) (squareTree right)

-- treeMap :: (a -> b) -> Tree a -> Tree b
-- treeMap f (Leaf x) = Leaf (f x)
-- treeMap f (Branch left right) = Branch (treeMap f left) (treeMap f right)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

squareTree' :: (Num a) => Tree a -> Tree a
squareTree' = fmap (\x -> x^2)

main :: IO ()
main = do
  threeBranch <- return $ Branch (Leaf 3) (Leaf 4)
  twoBranch   <- return $ Branch (Leaf 2) threeBranch
  putStrLn $ show $ squareTree' $ Branch (Leaf 1) twoBranch
