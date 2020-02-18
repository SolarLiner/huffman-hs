-- Copyright (c) 2020 Nathan Graule
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

module Huffman.GraphViz
  ( toGraph
  )
where
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Types.Generalised
                                               as G
import           Data.GraphViz.Types.Monadic
import           Data.Text.Lazy                as L
import           Data.Word
import           Text.Printf
import           Huffman.Base

graphStep :: HTree Char -> String -> DotM L.Text String
graphStep l@(Leaf f c) i =
  let s  = pack $ formatNode l i
      si = pack i
  in  do
        node si [textLabel s, shape BoxShape]
        return i

graphStep n@(Node f l r) i =
  let s  = pack $ formatNode n i
      si = pack i
  in  do
        sl <- graphStep l (i ++ "0")
        sr <- graphStep r (i ++ "1")
        node si [textLabel s]
        si --> L.pack sl
        si --> L.pack sr
        return i

toGraph :: Text -> HTree Char -> G.DotGraph L.Text
toGraph name t = digraph (Str name) $ graphStep t "0"

formatNode :: HTree Char -> String -> String
formatNode (Leaf f c  ) = printf "%c (#: %d): %s" c f
formatNode (Node f _ _) = printf "(#: %d): %s" f
