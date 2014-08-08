data Tree a = TreeLeaf a | TreeNode (Tree a) (Tree a) deriving (Show, Eq)

leaves :: Tree a -> [a]
leaves (TreeLeaf a) = [a]
leaves (TreeNode left right) = leaves left ++ leaves right

main :: IO ()
main = do
  x <- return $ TreeNode (TreeNode (TreeLeaf 1) (TreeLeaf 2)) (TreeNode (TreeLeaf 3) (TreeLeaf 4))
  putStrLn $ show $ leaves x
  putStrLn $ show $ leaves (TreeNode x x)
