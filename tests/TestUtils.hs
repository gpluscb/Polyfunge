module TestUtils where

import System.Directory (doesFileExist, listDirectory)
import Test.HUnit (assertFailure)
import Utils (concatMapM)

eitherAssertion :: (Show a) => Either a b -> IO ()
eitherAssertion (Left e) = assertFailure $ show e
eitherAssertion _ = return ()

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
