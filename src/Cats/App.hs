module Cats.App
  ( cats
  ) where

import           Cats.Parser           (allImportDeclarations)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.Directory.Tree (AnchoredDirTree (..), DirTree (..),
                                        readDirectoryWith)
import qualified Text.Parsec           as Parsec

cats :: FilePath -> IO ()
cats dir = do
  anchTree <- readDirectoryWith (withFile imports) dir
  case sequence (dirTree anchTree) of
    Left _        -> printTree anchTree
    Right dirtree -> printTree (dir :/ clean dirtree)

printTree :: Show a => AnchoredDirTree a -> IO ()
printTree = mapM_ putStrLn . drawAnchoredTree

withFile :: (T.Text -> a) -> FilePath -> IO a
withFile f filepath = f <$> TIO.readFile filepath

imports :: T.Text -> Either Parsec.ParseError [T.Text]
imports input =
  case Parsec.runParser allImportDeclarations () "" (T.unpack input) of
    Left parseErr -> Left parseErr
    Right result  -> Right (map T.pack result)

-- we only care about modules we're importing from within the project
clean :: DirTree [T.Text] -> DirTree [T.Text]
clean = fmap (filter relativePath)

relativePath :: T.Text -> Bool
relativePath = T.isPrefixOf "."

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
