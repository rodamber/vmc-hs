{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}

module PBEncoder where

import Control.Monad
import Control.Monad.Trans.Reader
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

generalizedTotalizer :: PBEncoding
generalizedTotalizer (PB wxs k) = do
  -- cnf <- encodeTree <$> build wxs
  undefined
  -- let kLit = maximum (concat cnf) -- FIXME: Wrong!
  -- return ([-kLit] : cnf)

data BTree a = L a | N (BTree a) a (BTree a)
  deriving (Functor, Foldable, Traversable)

type BT = BTree [WL]

instance Show a => Show (BTree a) where
  show = showTree 0

showTree :: Show a => Int -> BTree a -> String
showTree _ (L xs)     = "L " ++ show xs
showTree n (N l xs r) = "N (" ++ showTree (n + 3) l ++ ")\n" ++
                        iden n ++ "  (" ++ show xs ++ ")\n" ++
                        iden n ++ "  (" ++ showTree (n + 3) r ++ ")"
  where
    iden n = (replicate n ' ')


--------------------------------------------------------------------------------

-- g n xss = filter (<= n+1) $
g :: (Traversable t, Num a, Eq a) => t [a] -> [a]
g xss = nub $ (concat xss) ++ map sum (sequence xss)

chunks :: Integral t => t -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = ys : chunks n zs
  where (ys,zs) = genericSplitAt n xs

level0 :: [a] -> [[a]]
level0 = map (:[])

levelUp :: (Num a, Eq a) => [[a]] -> [[a]]
levelUp = fmap g . chunks 2

levels :: (Num a, Eq a) => [a] -> [[[a]]]
levels xss = map ($ xss') (scanr1 (.) (replicate height levelUp)) ++ [xss']
  where xss' = level0 xss
        height = ceiling $ logBase 2 $ genericLength xss

-- Rewrite levels with this?
iterateN f n = genericTake n . iterate f

build :: (Num t, Eq t) => [t] -> BTree [t]
build = head . build' . levels

build' :: [[a]] -> [BTree a]
build' (xs:ys:t) = zipWith mkNode xs (chunks 2 (build' (ys:t)))
build' [xs] = map L xs

mkNode :: a -> [BTree a] -> BTree a
mkNode z [x,y] = N x z y

-- FIXME: This forgets the original literals!
totalizerTree :: [Int] -> Encoder (BTree [WL])
totalizerTree xs =
  let tree       = build xs
      mkLiterals = traverse (\x -> newLit >>= \l -> return (x, l))
  in traverse mkLiterals tree

value :: BTree a -> a
value (L x)     = x
value (N _ x _) = x

encodeTree :: BTree [WL] -> CNF
encodeTree (L _) = []
encodeTree (N lhs wls rhs) =
  let lvalues = value lhs
      rvalues = value rhs

      lookup' x m = fromJust (lookup x m)

      simpleClauses = (flip map) (lvalues ++ rvalues) $
                      \(weight, lit) -> [-lit, lookup' weight wls]

      sumClauses = do
        (wl, ll) <- lvalues
        (wr, rl) <- rvalues
        return [-ll, -rl, lookup' (wl + wr) wls]
  in concat [simpleClauses, sumClauses, encodeTree lhs, encodeTree rhs]
