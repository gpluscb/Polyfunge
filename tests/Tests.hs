import qualified System.Exit as Exit
import Test.HUnit (Counts (failures), Test (TestList), runTestTT)

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0
    then Exit.exitFailure
    else Exit.exitSuccess

tests :: Test
tests = TestList []
