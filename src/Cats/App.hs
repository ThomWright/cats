module Cats.App
  ( cats
  ) where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.Directory.Tree (AnchoredDirTree (..), DirTree (..),
                                        readDirectoryWith)

cats :: FilePath -> IO ()
cats dir = do
  anchTree <- readDirectoryWith (withFile imports) dir
  mapM_ putStrLn $ drawAnchoredTree anchTree

withFile :: (T.Text -> a) -> FilePath -> IO a
withFile f filepath = f <$> TIO.readFile filepath

imports :: T.Text -> [T.Text]
imports = filter (T.isPrefixOf "import") . T.lines

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
