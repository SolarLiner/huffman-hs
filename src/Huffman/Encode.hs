-- Copyright (c) 2020 Nathan Graule
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Huffman.Encode
  ( encode
  )
where
import           Control.Applicative
import           Data.Binary             hiding ( encode )
import           Data.Maybe
import           Huffman.Base

encode :: String -> Maybe ([(Char, [Bool])], [Bool])
encode s =
  let h = huffman s
      l = buildAssocList $ assignCode h
      a = concat <$> mapM (`lookup` l) s
  in  fmap (l, ) a


buildAssocList :: HTree (a, [Bool]) -> [(a, [Bool])]
buildAssocList = \case
  Leaf _ (c, i) -> [(c, i)]
  Node _ l r    -> buildAssocList l ++ buildAssocList r

assignCode :: HTree a -> HTree (a, [Bool])
assignCode = step [False]
 where
  step i = \case
    Leaf f c   -> Leaf f (c, i)
    Node f l r -> Node f (step (i ++ [False]) l) (step (i ++ [True]) r)
