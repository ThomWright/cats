module Cats.App
  ( cats
  ) where

import           Data.ByteString       (ByteString, hGetLine)
import           System.Directory.Tree (AnchoredDirTree (..), DirTree (..),
                                        readDirectoryWith)
import           System.IO             (Handle, IOMode (ReadMode), withFile)

cats :: FilePath -> IO ()
cats dir = do
  anchTree <- readDirectoryWith readFirstLine dir
  mapM_ putStrLn $ drawAnchoredTree anchTree

readFirstLine :: FilePath -> IO ByteString
readFirstLine fp = withFile fp ReadMode readL
  where
    readL :: Handle -> IO ByteString
    readL = hGetLine

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
