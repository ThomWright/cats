module Cats.DirTree.Utils
  ( mapFilesWithPath
  , mapFiles
  , filterFiles
  ) where

import           System.Directory.Tree (DirTree (..))

mapFiles :: (a -> b) -> DirTree a -> DirTree b
mapFiles f (Dir name' files)      = Dir name' (fmap (mapFiles f) files)
mapFiles f (File name' contents') = File name' (f contents')
mapFiles _ (Failed name' e)       = Failed name' e

mapFilesWithPath :: (FilePath -> a -> b) -> DirTree a -> DirTree b
mapFilesWithPath f (Dir name' files) =
  Dir name' (fmap (mapFilesWithPath f) files)
mapFilesWithPath f (File name' contents') = File name' (f name' contents')
mapFilesWithPath _ (Failed name' e) = Failed name' e

filterFiles :: (a -> Bool) -> DirTree a -> DirTree a
filterFiles f (Dir name' files') =
  Dir name' (filter (pred' f) (map (filterFiles f) files'))
filterFiles _ other = other

pred' :: (a -> Bool) -> DirTree a -> Bool
pred' f (Dir _ files')     = any (pred' f) files'
pred' f (File _ contents') = f contents'
pred' _ (Failed _ _)       = False
