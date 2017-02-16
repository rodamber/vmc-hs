module Main where

import Control.Monad (mapM_)
import Data.List (sortBy, nub)
import qualified Data.Bimap as BM
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Tuple (swap)
import System.Environment (getArgs)

import Picosat

import Sat
import Parser
import Types

constraints :: Int -> [Encoder CNF]
constraints n =
  [ atLeastOne
  , atMostOne bitwise
  , antiCollocation bitwise
  , capacityLimit generalizedTotalizer
  , serverUpperLimit n generalizedTotalizer
  ]

encoder :: [Encoder CNF] -> Encoder CNF
encoder es = concat <$> sequence es

cnf :: Environment -> Int -> CNF
cnf env n = encode env (encoder (constraints n))

type Assignment = [(VM,Server)]

solution2assignment :: Environment -> Solution -> Maybe Assignment
solution2assignment env (Solution sol) =
  let lookup = flip BM.lookup (bimap env)
  in Just
     $ sortBy (comparing (jobID . fst))
     $ sortBy (comparing (vmIndex . fst))
     $ map swap
     $ mapMaybe lookup sol
solution2assignment _ _ = Nothing

output :: Assignment -> IO ()
output a = do
  putStrLn $ "o " ++ show optimumValue
  mapM_ putVS a

  where
    optimumValue = length $ nub $ snd <$> a

    putVS (v,s) = putStrLn . concat $
      [ show (jobID v)
      , " "
      , show (vmIndex v)
      , " -> "
      , show (serverID s)
      ]

main = (!!0) <$> getArgs >>= main'

main' fileName = do
  Just problem <- parse fileName
  let env = populate problem
  Just a <- solution2assignment env <$> main'' env
  output a

main'' :: Environment -> IO Solution
main'' env = loop (length $ servers env) Unknown
  where
    loop :: Int -> Solution -> IO Solution
    loop n previousSolution =
      case previousSolution of
        Solution _ -> check
        Unknown    -> check
        Unsatisfiable -> error "loop: unsat"
      where
        check :: IO Solution
        check = do
          newSolution <- solve (cnf env n)
          case newSolution of
            Unsatisfiable -> return previousSolution
            Solution _    -> loop (n-1) newSolution
            Unknown -> error "check: unknown"

