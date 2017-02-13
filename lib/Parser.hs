{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Text.Trifecta

import Types

parseServer :: Parser Server
parseServer = do
  [serverID, ramCap, cpuCap] <- (fmap fromInteger) <$> count 3 integer
  return $ Server serverID ramCap cpuCap

parseVM :: Int -> Parser VM
parseVM vmID = do
  [jobID,vmIndex,ramReq,cpuReq] <- (fmap fromInteger) <$> count 4 integer
  hasAntiCollocation <- read <$> (string "True" <|> string "False")
  whiteSpace
  return $ VM vmID jobID vmIndex ramReq cpuReq hasAntiCollocation

parseInput :: Parser Problem
parseInput = do
  serverCount <- fromInteger <$> integer
  servers <- count serverCount parseServer
  vmCount <- fromInteger <$> integer
  vms <- sequence (parseVM <$> [0 .. vmCount - 1])
  return (servers,vms)

parse :: String -> IO (Maybe Problem)
parse = parseFromFile parseInput
