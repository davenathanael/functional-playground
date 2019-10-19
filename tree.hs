module CTree where

data BinaryTree a = Leaf a | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

jmlNode :: BinaryTree a -> Int
jmlNode (Leaf _) = 1
jmlNode (Branch b1 n b2) = jmlNode b1 + jmlNode b2 + 1