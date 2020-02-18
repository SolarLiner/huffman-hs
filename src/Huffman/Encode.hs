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
import           Data.Bits                      ( shiftL )
import           Huffman.Base

encode :: String -> Maybe ([(Char, [Word8])], [Word8])
encode s =
  let h = huffman s
      l = buildAssocList $ assignCode h
      a = concat <$> mapM (`lookup` l) s
  in  (encodeAssocList l, ) . pack <$> a

encodeAssocList []            = []
encodeAssocList ((c, x) : xs) = (c, pack x) : encodeAssocList xs

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

pack :: [Bool] -> [Word8]
pack xs =
  let one l =
          foldr (\(i, x) acc -> acc + x `shiftL` i) 0
            $ fstM (7 -)
            $ enumerate
            $ map b2b l
  in  map one $ group False 8 xs

group _ _ [] = []
group d n l
  | length l > n = take n l : group d n (drop n l)
  | n <= 0       = error "n needs to be positive"
  | otherwise    = let rest = n - length l in [l ++ [ d | _ <- [1 .. rest] ]]

enumerate :: [a] -> [(Int, a)]
enumerate = e' 0
 where
  e' i []       = []
  e' i (x : xs) = (i, x) : e' (i + 1) xs

fstM f = \case
  []            -> []
  ((a, i) : xs) -> ((f a, i) : fstM f xs)

b2b :: Bool -> Word8
b2b False = 0
b2b True  = 1
