module TestUtils where

import System.Directory (doesFileExist, listDirectory)
import Utils (concatMapM)

readFilesInDirRecursive :: FilePath -> IO [(FilePath, String)]
readFilesInDirRecursive dirPath = do
  entries <- map (dirPath ++) <$> listDirectory dirPath
  contents <-
    concatMapM
      ( \path -> do
          isFile <- doesFileExist path
          if isFile
            then
              (\contents -> [(path, contents)]) <$> readFile path
            else
              -- Assuming this is directory, might not be true but eh
              readFilesInDirRecursive (path ++ "/")
      )
      entries
  return contents
