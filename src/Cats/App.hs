module Cats.App
  ( cats
  ) where

import           Cats.DirTree.Print    (printTree)
import           Cats.DirTree.Utils    (filterFiles, mapFiles)
import           Data.List             (isSuffixOf)
import           Data.Maybe

import           Cats.Parser           (allImportDeclarations)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           System.Directory.Tree (AnchoredDirTree (..), DirTree (..),
                                        readDirectoryWith)
import qualified Text.Parsec           as Parsec

cats :: FilePath -> IO ()
cats dir = do
  fileContents <- readDirectoryWith readTSFile dir
  let onlyTS = filterFiles isJust (dirTree fileContents)
  case sequence onlyTS of
    Nothing -> putStrLn "oh no"
    Just tsFiles ->
      let parsed = mapFiles parseImports tsFiles
      in case sequence parsed of
           Left _        -> printTree (dir :/ parsed)
           Right dirtree -> printTree (dir :/ onlyRelative dirtree)

isTypeScriptFile :: FilePath -> Bool
isTypeScriptFile filepath' = ".ts" `isSuffixOf` filepath'

readTSFile :: FilePath -> IO (Maybe T.Text)
readTSFile filepath' =
  if isTypeScriptFile filepath'
    then fmap Just (TIO.readFile filepath')
    else return Nothing

parseImports :: T.Text -> Either Parsec.ParseError [T.Text]
parseImports input =
  case Parsec.runParser allImportDeclarations () "" (T.unpack input) of
    Left parseErr -> Left parseErr
    Right result  -> Right (map T.pack result)

-- we only care about modules we're importing from within the project
onlyRelative :: DirTree [T.Text] -> DirTree [T.Text]
onlyRelative = fmap (filter relativePath)

relativePath :: T.Text -> Bool
relativePath path = "." `T.isPrefixOf` path
