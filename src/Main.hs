-- Copyright (c) 2020 Nathan Graule
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Main where
import           Huffman.Base
import           Huffman.Encode
-- import           Huffman.GraphViz
import qualified Data.ByteString.Lazy          as BIN
import           Data.GraphViz
import           Data.GraphViz.Printing
import           Data.Text.Lazy
import           Data.Maybe
import           Data.Binary                    ( encodeFile )
import           System.IO
import           System.Environment

main :: IO ()
{- main =
  interact (unpack . renderDot . toDot . toGraph (pack "Huffman") . huffman) -}
main = do
  [inp, out] <- getArgs
  s          <- readFile inp
  encodeFile out $ encode s
