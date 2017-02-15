module PBEncoder where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe

import Encoder
import Types

-- Weighted literal
type WL = (Int,Lit)
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
generalizedTotalizer (PB wxs k) = undefined

data BT = L [WL] | N BT [WL] BT

instance Show BT where
  show = showTree 0

showTree :: Int -> BT -> String
showTree _ (L xs)     = "L " ++ show xs
showTree n (N l xs r) = "N (" ++ showTree (n + 3) l ++ ")\n" ++
                        iden n ++ "  (" ++ show xs ++ ")\n" ++
                        iden n ++ "  (" ++ showTree (n + 3) r ++ ")"
  where
    iden n = (replicate n ' ')

value :: BT -> [WL]
value (L x)     = x
value (N _ x _) = x

weights :: BT -> [Int]
weights = map fst . value

half :: Integral b => [a] -> b
half = ceiling . (/2) . genericLength

halve :: [a] -> ([a],[a])
halve xs = splitAt (half xs) xs

build :: [WL] -> Encoder BT
build [x,y,z]  = build [x,y] >>= buildNode (L [z])
build [x,y]    = buildNode (L [x]) (L [y])
build [x]      = return (L [x])
build []       = error "build: no literals"
build xs       = do let (ys,zs) = halve xs
                    ny <- build ys
                    nz <- build zs
                    buildNode ny nz

buildNode :: BT -> BT -> Encoder BT
buildNode xs ys = do
  let xWeights   = weights xs
      yWeights   = weights ys
      newWeights = nub $ xWeights ++ yWeights ++
                   map sum (sequence [xWeights, yWeights])

  (min,max) <- fromJust $ moreLits (length newWeights)

  let newWeightedLiterals = zip newWeights [min..max]
  return $ N xs newWeightedLiterals ys


--------------------------------------------------------------------------------

-- g n xss = filter (<= n+1) $
g xss =
  nub $ (concat xss) ++  map sum (sequence xss)

chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

type Level a = [[a]]

-- levelUp :: (Ord a, Num a) => a -> Level a -> Level a
-- levelUp n = fmap (g n) . chunks 2
levelUp = fmap g . chunks 2

-- levels :: (Ord a, Num a) => a -> Level a -> [Level a]
-- levels n xss = xss : (map ($ xss) $ scanl1 (.) (replicate height (levelUp n)))
levels xss = xss : (map ($ xss) $ scanl1 (.) (replicate height levelUp))
  where
    height = ceiling $ logBase 2 $ genericLength xss

-- level0 = [[2],[3],[3],[3]]
-- level1 = levelUp 5 level0 -- [[2,3,5],[3,6]]
-- level2 = levelUp 5 level1 -- [[2,3,5,6]]
-- level3 = levelUp 5 level2 -- [[2,3,5,6]]

