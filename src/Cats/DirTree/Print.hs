module Cats.DirTree.Print
  ( printTree
  ) where

import           System.Directory.Tree (AnchoredDirTree (..), DirTree (..))

printTree :: Show a => AnchoredDirTree a -> IO ()
printTree = mapM_ putStrLn . drawAnchoredTree

drawAnchoredTree :: Show a => AnchoredDirTree a -> [String]
drawAnchoredTree (anchor' :/ tree) = anchor' : drawDirTree tree

drawDirTree :: Show a => DirTree a -> [String]
drawDirTree (File name' file') = [name', show file']
drawDirTree (Failed name' err') = [name', show err']
drawDirTree (Dir name' contents') = name' : drawSubTrees contents'
  where
    drawSubTrees [] = []
    drawSubTrees [t] = "|" : shift "`- " "   " (drawDirTree t)
    drawSubTrees (t:ts) =
      "|" : shift "+- " "|  " (drawDirTree t) ++ drawSubTrees ts
    shift first other = zipWith (++) (first : repeat other)
