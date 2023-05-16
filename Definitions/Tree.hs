module Tree ( Tree (Leaf, Node) ) where
  -- Definition of the example data structure: Tree
  data Tree = Leaf | Node Int Tree Tree

  instance Show Tree where show t = '\n' : unlines (pt t)
  
  pt (Node b Leaf Leaf) = ["N: " ++ show b ++ " -> L L"]
  pt (Node b left right)
      = ("N: " ++ show b) : pt_subtree left right
          where
              pt_subtree left right =
                  pad "|- " "|  " (pt left)
                      ++ pad "`- " "   " (pt right)
              pad first rest = zipWith (++) (first : repeat rest)
  pt Leaf
      = ["L"]