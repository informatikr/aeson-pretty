{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main (main) where

import Prelude hiding (interact, concat, unlines, null)
import Data.Aeson (Value(..), json', encode)
import Data.Aeson.Encode.Pretty
import Data.Attoparsec.Lazy (Result(..), parse)
import Data.ByteString.Lazy.Char8 (ByteString, interact, unlines, null)
import Data.Version (showVersion)
import Paths_aeson_pretty (version)
import System.Console.CmdArgs


data Options = Opts { compact :: Bool
                    , indent  :: Int
                    , sort    :: Bool
                    }
    deriving (Data, Typeable)

opts :: Options
opts = Opts
    { compact = False &= help "Compact output."
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
    , "(c) Falko Peters 2011"
    , ""
    , "License: BSD3, for details see the source-repository at"
    , "http://www.github.com/informatikr/aeson-pretty."
    , ""
    ]

main :: IO ()
main = do
    Opts{..} <- cmdArgs opts
    let conf = Config { confIndent  = indent
                      , confCompare = if sort then compare else mempty
                      }
        enc = if compact then encode else encodePretty' conf
    interact $ unlines . map enc . values

values :: ByteString -> [Value]
values s = case parse json' s of
            Done rest v     -> v : values rest
            Fail rest _ _
                | null rest -> []
                | otherwise -> error "invalid json"
