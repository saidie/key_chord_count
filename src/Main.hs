module Main (
  main
) where

import Data.Monoid (mconcat)
import System.Environment (getArgs)
import System.IO

import ChordHist

loadChordHist path = do
  handle <- openFile path ReadMode
  content <- hGetContents handle
  content `seq` hClose handle
  return $ chordHist content

main = do
  getArgs >>= mapM loadChordHist >>= putStrLn . show . mconcat
