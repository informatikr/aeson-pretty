{-# LANGUAGE OverloadedStrings #-}

import           Test.Framework
import           Test.Framework.Providers.HUnit

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy      as B
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import           Data.Map                       (Map)
import           Test.HUnit                     (assertEqual)
import           System.FilePath.Posix
import           Data.String.Utils (rstrip)

testDataDir :: FilePath
testDataDir = "test/data/suppress-nulls"

eitherDecodeMap :: IO (Map String (Maybe String))
eitherDecodeMap = do
  d <- eitherDecode <$> B.readFile (testDataDir </> "input.json")
  case d of
    Left err -> error $ "ERROR: " ++ err
    Right val -> return val


prettifyMap :: Bool -> Map String (Maybe String) -> String
prettifyMap s m = U.toString $ encodePretty' (Config 4 compare s) m


testEquality :: Bool -> FilePath -> IO ()
testEquality suppress data_filename = do
  vals <- eitherDecodeMap
  let pretty_output_computed = prettifyMap suppress vals
  reference_output_file_content <- readFile $ testDataDir </> data_filename
  let pretty_output_expected = rstrip $ reference_output_file_content

  assertEqual "Checking equality..." pretty_output_expected pretty_output_computed


tests = [
    testGroup "Null value suppression" [
        testCase "nulls-allowed" $ testEquality True "null-values-allowed.json"
      , testCase "nulls-suppressed" $ testEquality False "null-values-suppressed.json"
      ]
  ]


main = defaultMain tests
