{-# OPTIONS -XFlexibleContexts #-}
module Main (
  main
) where

import Data.Bifunctor
import Data.Char
import qualified Data.Map.Lazy as M
import System.IO
import System.Environment
import qualified Data.ListLike as LL

type ChordHist = M.Map (Char, Char) Int

addChordHist :: ChordHist -> ChordHist -> ChordHist
addChordHist = M.unionWith (+)

pairs :: LL.ListLike l e => l -> [(e, e)]
pairs l
  | LL.null l = []
  | otherwise = pairs' [] (LL.head l) (LL.tail l)
  where
    pairs' res e l
      | LL.null l = res
      | otherwise =
        let (e', l') = (LL.head l, LL.tail l)
        in pairs' ((e, e'):res) e' l'

hist :: (Ord e, LL.ListLike l e) => l -> [(e, Int)]
hist l
  | LL.null l = []
  | otherwise = let l' = LL.sort l in hist' [] (LL.head l') 1 (LL.tail l')
  where
    hist' res e cnt l
      | LL.null l = (e, cnt) : res
      | otherwise =
        let (e', l') = (LL.head l, LL.tail l)
        in if e == e'
           then hist' res e (cnt+1) l'
           else hist' ((e, cnt) : res) e' 1 l'

chordHist :: LL.ListLike str Char => str -> ChordHist
chordHist =
  M.fromList . hist . normalize . pairs
  where
    normalize = map (bimap1 toLower) . filter (andTup2 . bimap1 isChord)
    isChord ch = ord ch <= 0xFF && isAlpha ch

bimap1 f = bimap f f

andTup2 (True, True) = True
andTup2 _ = False

main = do
  getArgs >>= mapM loadChordHist >>= print . foldl addChordHist M.empty
  where
    loadChordHist path = do
      handle <- openFile path ReadMode
      content <- fmap chordHist $ hGetContents handle
      content `seq` hClose handle
      return content
