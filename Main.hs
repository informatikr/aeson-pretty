{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import Prelude hiding (interact, concat)
import Data.Aeson (Value(..), json, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Attoparsec.Lazy (Result(..), parse)
import Data.ByteString.Lazy (ByteString, interact, concat, append)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy as L (null)
import System.Console.CmdArgs


data Options = Opts { compact :: Bool } deriving (Data, Typeable)

opts :: Options
opts = Opts { compact = False &= help "Compact output. Ignores the -i flag." 
                              &= groupname "Flags"}
        &= program "aeson-pretty"
        &= summary "aeson-pretty 0.1: Pretty JSON, the easy way."
        &= details info

info :: [String]
info =
    [ "Read JSON from stdin and pretty-print to stdout. The complementary "
    , "compact-mode removes whitespace from the input."
    , ""
    , "(c) Falko Peters 2011"
    , ""
    , "License: BSD3, for details see the source-repository at"
    , "http://www.github.com/informatikr/aeson-pretty"
    , ""
    ]

main :: IO ()
main = do
    Opts{..} <- cmdArgs opts
    let enc = if compact then encode else encodePretty
    interact (concat . map ((`append` pack "\n") . enc) . values)

values :: ByteString -> [Value]
values s = case parse json s of
            Done rest v       -> v : values rest
            Fail rest _ _
                | L.null rest -> []
                | otherwise   -> error "invalid json"
