-- Copyright (c) 2020 Nathan Graule
--
-- This software is released under the MIT License.
-- https://opensource.org/licenses/MIT

{-# LANGUAGE LambdaCase #-}
module Huffman.Base
  ( HTree(Node, Leaf)
  , huffman
  )
where
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.List
import           Data.Function
import           System.IO

intOfBool False = 0
intOfBool True  = 1

data HTree a =
    Node Int (HTree a) (HTree a)
  | Leaf Int a
  deriving(Show, Eq)

hFreq = \case
  Leaf f _   -> f
  Node f _ _ -> f

countUniq :: (Eq a, Ord a) => [a] -> [(a, Int)]
countUniq s = sortBy (compare `on` snd) $ Map.assocs $ foldr
  (\x acc -> Map.insert x (Map.findWithDefault 0 x acc + 1) acc)
  Map.empty
  s

hSort = sortBy (compare `on` hFreq)

hStep = \case
  (l@(Leaf lf _) : r@(Leaf rf _) : xs) -> (Node (lf + rf) l r) : (hStep xs)
  (l@(Node lf _ _) : r@(Node rf _ _) : xs) -> (Node (lf + rf) l r) : (hStep xs)
  (l@(Node lf _ _) : r@(Leaf rf _) : xs) -> (Node (lf + rf) l r) : (hStep xs)
  (l@(Leaf lf _) : r@(Node rf _ _) : xs) -> (Node (lf + rf) l r) : (hStep xs)
  [a] -> [a]
  []  -> []

huffman :: String -> HTree Char
huffman s =
  head
    $ until ((== 1) . length) hStep
    $ hSort
    $ map (uncurry (flip Leaf))
    $ countUniq s
