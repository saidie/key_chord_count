{-# OPTIONS -XFlexibleContexts #-}
module ChordHist (
  chordHist
) where

import Data.Bifunctor (bimap)
import Data.Char (ord, isAlpha, toLower)
import qualified Data.ListLike as LL
import qualified Data.Map.Lazy as M
import Data.Monoid (Monoid(..))
import Text.Printf (printf)

newtype ChordHist = CH (M.Map (Char, Char) Int)

instance Monoid ChordHist where
  mempty = CH M.empty
  mappend (CH a) (CH b) = CH $ M.unionWith (+) a b

instance Show ChordHist where
  show (CH m) =
    let show' (c1, c2) cnt = printf "%d %c%c" cnt c1 c2
    in unlines . M.elems . M.mapWithKey show' $ m

bimap1 f = bimap f f

andTup2 (True, True) = True
andTup2 _ = False

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
  CH . M.fromList . hist . normalize . pairs
  where
    normalize = map (bimap1 toLower) . filter (andTup2 . bimap1 isChord)
    isChord ch = ord ch < 0x80 && isAlpha ch
