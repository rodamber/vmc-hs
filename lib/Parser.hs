{-# LANGUAGE OverloadedStrings #-}

module Parser where

-- import qualified Data.Text as T
-- import qualified Data.Text.Read as T
-- import qualified Data.Text.IO as T

-- import Types

-- parse :: T.Text -> Problem
-- parse input = Problem servers vms
--   where
--     lines       = T.lines input
--     serverCount = toInt (head lines)
--     servers     = map readServer (take serverCount (drop 1 lines))
--     vms         = map readVM (drop (serverCount + 2) lines)

-- toInt :: Integral a => T.Text -> a
-- toInt x = either (const 0) fst (T.decimal x)

-- readServer :: T.Text -> Server
-- readServer t = Server sid cpu ram
--   where [sid, cpu, ram] = toInt <$> (T.words t)

-- readVM :: T.Text -> VM
-- readVM t = undefined
