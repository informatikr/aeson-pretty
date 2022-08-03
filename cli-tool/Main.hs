{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main (main) where

import Prelude hiding (interact, concat, unlines, null)
import Control.Exception (bracketOnError)
import Control.Monad (forM_)
import Data.Aeson (Value(..), json', encode)
import Data.Aeson.Encode.Pretty
import Data.Attoparsec.Lazy (Result(..), parse)
import Data.ByteString.Lazy.Char8 (ByteString, interact, unlines, null, hPut, hGetContents)
import Data.Version (showVersion)
import Paths_aeson_pretty (version)
import System.Console.CmdArgs
import System.Directory (removeFile, renameFile)
import System.FilePath (splitFileName, replaceExtension)
import System.IO (IOMode(ReadMode), hClose, openBinaryTempFileWithDefaultPermissions, withFile)


data Options = Opts { source  :: [FilePath]
                    , compact :: Bool
                    , indent  :: Int
                    , sort    :: Bool
                    }
    deriving (Data, Typeable)

opts :: Options
opts = Opts
    { source  = def   &= args
    , compact = False &= help "Compact output."
    , indent  = 4     &= help "Number of spaces per nesting-level (default 4)."
    , sort    = False &= help "Sort objects by key (default: undefined order)."
    }   &= program prog
        &= summary smry
        &= details info
  where
    prog = "aeson-pretty"
    smry = prog++" "++showVersion version++": Pretty JSON, the easy way."

info :: [String]
info =
    [ "Read JSON from stdin and pretty-print to stdout. The complementary "
    , "compact-mode removes whitespace from the input."
    , ""
    , "If one or more files are specified, the files will be prettified in place. No backups are made."
    , ""
    , "(c) Falko Peters 2011"
    , ""
    , "License: BSD3, for details see the source-repository at"
    , "http://www.github.com/informatikr/aeson-pretty."
    , ""
    ]

main :: IO ()
main = do
    Opts{..} <- cmdArgs opts
    let conf = Config { confIndent          = Spaces indent
                      , confCompare         = if sort then compare else mempty
                      , confNumFormat       = Generic
                      , confTrailingNewline = False
                      }
        enc = if compact then encode else encodePretty' conf
    case source of
      [] -> interact $ unlines . map enc . values
      _ -> forM_ source (\path -> inplace path $ unlines . map enc . values)

values :: ByteString -> [Value]
values s = case parse json' s of
            Done rest v     -> v : values rest
            Fail rest _ _
                | null rest -> []
                | otherwise -> error "invalid json"

inplace :: FilePath -> (ByteString -> ByteString) -> IO ()
inplace path interaction = do
    tempPath <- bracketOnError acquire failure execute
    renameFile tempPath path
  where
    (tempDir, tempName) = splitFileName path
    tempPattern = replaceExtension tempName ".XXXXX"
    acquire = openBinaryTempFileWithDefaultPermissions tempDir tempPattern
    execute (tempPath, tempHandle) = do
        withFile path ReadMode $ \readHandle -> do
            contents <- hGetContents readHandle
            hPut tempHandle $ interaction contents
        hClose tempHandle
        return tempPath
    failure (tempPath, tempHandle) = do
        hClose tempHandle
        removeFile tempPath

