{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TupleSections     #-}

module PBEncoder where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.Bits
import Data.List
import Data.Maybe

import Encoder
import Types

-- Weighted literal
type WL = (Int,Lit)

weight :: WL -> Int
weight = fst

lit :: WL -> Int
lit = snd

-- Pseudo-boolean expression
data PBExpr = PB [WL] Int

type PBEncoding = PBExpr -> Encoder CNF

--------------------------------------------------------------------------------
-- Sequential Weighted Counter

sequentialWeightedCounter :: PBEncoding
sequentialWeightedCounter (PB wxs k) = do
  let n = length wxs
  (min,max) <- fromJust $ moreLits (k * n)

  let x i = (map snd wxs) !! (i - 1)
  let w i = (map fst wxs) !! (i - 1)
  let s i j = [min..max] !! ((i - 1) * k + (j - 1))

  return $
    [[- x i, s i j] | i <- [1..n], j <- [1..w i]]
    ++ [[- s 1 j] | j <- [w 1 + 1..k]]
    ++ [[-s (i-1) j , s i j] | i <- [2..n], j <- [1..k]]
    ++ [[- x i, -s (i-1) j, s i (j + w i)] | i <- [2..n], j <- [1..k - w i]]
    ++ [[- x i, -s (i-1) (k + 1 - w i)] | i <- [2..n]]

--------------------------------------------------------------------------------
-- Generalized Totalizer
-- Paper describing the encoding: https://arxiv.org/pdf/1507.05920.pdf

-- FIXME: @see totalizerTree
generalizedTotalizer :: PBEncoding
generalizedTotalizer (PB wxs k) = totalizerTree wxs >>= return . encodeTree k

sums :: (Traversable t, Num a, Eq a) => t [a] -> [a]
sums xss = nub $ concat xss ++ fmap sum (sequence xss)

chunks :: Integral t => t -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = ys : chunks n zs
  where (ys,zs) = genericSplitAt n xs

level0 :: [a] -> [[a]]
level0 = map (:[])

levelUp :: (Num a, Eq a) => [[a]] -> [[a]]
levelUp = fmap sums . chunks 2

levels :: (Num a, Eq a) => [[a]] -> [[[a]]]
levels xss = ($ xss) <$> scanr1 (.) (replicate height levelUp)
  where height = ceiling $ logBase 2 $ genericLength xss

build :: [[a]] -> BTree a
build = head . build'

build' :: [[a]] -> [BTree a]
build' [xs] = map L xs
build' (xs:ys:t) = zipWith mkNode xs (chunks 2 (build' (ys:t)))
  where mkNode z [x,y] = N x z y

tree :: [WL] -> Encoder (BTree [WL])
tree xs = labeled >>= return . build . (++ [level0 xs])
  where
    ws = level0 (map fst xs)
    labeled = (traverse . traverse . traverse) label (levels ws)

-- FIXME: The encoding spec does not specify how to structure the tree if the
-- formula size is not a power of two, so we're left wondering...
-- Our approach was to divide the input list in smaller lists, with the size of
-- each being a power of two and then merging them.
totalizerTree :: [WL] -> Encoder (BTree [WL])
totalizerTree xs = mergeAll (tree <$> splitXs)
  where splitXs = takes ((2^) <$> flags (length xs)) xs

mergeAll :: [Encoder (BTree [WL])] -> Encoder (BTree [WL])
mergeAll = head . mergeAll'

mergeAll' :: [Encoder (BTree [WL])] -> [Encoder (BTree [WL])]
mergeAll' (x:y:ys) = mergeAll' $ (merge x y) : (mergeAll' ys)
mergeAll' xs = xs

merge :: Encoder (BTree [WL]) -> Encoder (BTree [WL]) -> Encoder (BTree [WL])
merge e1 e2 = do
  t1 <- e1
  t2 <- e2
  v' <- traverse label (sums $ fmap fst <$> value <$> [t1, t2])
  return (N t1 v' t2)

label :: t -> Encoder (t, Int)
label w = newLit >>= return . (w,)

takes xs ys = ($ys) <$> take <$> xs
flags x = filter (testBit x) [0 .. finiteBitSize (x::Int)]

encodeTree :: Int -> BTree [WL] -> CNF
encodeTree k node@(N _ wls _) = [[-lit] | (weight, lit) <- wls, weight > k]
                                ++ encodeTree' node

encodeTree' :: BTree [WL] -> CNF
encodeTree' (L _) = []
encodeTree' (N lhs wls rhs) =
  let lvalues = value lhs
      rvalues = value rhs

      lookup' x m = fromJust (lookup x m)

      simpleClauses = (flip map) (lvalues ++ rvalues) $
                      \(weight, lit) -> [-lit, lookup' weight wls]

      sumClauses = do
        (wl, ll) <- lvalues
        (wr, rl) <- rvalues
        return [-ll, -rl, lookup' (wl + wr) wls]
  in concat [simpleClauses, sumClauses, encodeTree' lhs, encodeTree' rhs]


data BTree a = L a | N (BTree a) a (BTree a)
  deriving (Functor, Foldable, Traversable)

value :: BTree a -> a
value (L x)     = x
value (N _ x _) = x

instance Show a => Show (BTree a) where
  show = showTree 0

showTree :: Show a => Int -> BTree a -> String
showTree _ (L xs)     = "L " ++ show xs
showTree n (N l xs r) = "N (" ++ showTree (n + 3) l ++ ")\n" ++
                        iden n ++ "  (" ++ show xs ++ ")\n" ++
                        iden n ++ "  (" ++ showTree (n + 3) r ++ ")"
  where
    iden n = (replicate n ' ')

