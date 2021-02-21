{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DisjointSets (module DisjointSets) where

import Flow
import qualified Data.Map as Map
import qualified Data.Foldable as F
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Text.Prettyprint.Doc


newtype DisjointSets a 
    = DisjointSets (Map.Map a (Set.Set a))
    deriving (Eq, Ord, Show, Monoid)

instance (Ord a, Pretty a) => Pretty (DisjointSets a) where
    pretty = pretty . elems 

instance (Pretty a) => Pretty (Set.Set a) where
    pretty = pretty . Set.toList

fromSets :: (Ord a, Foldable f) => f (Set.Set a) -> DisjointSets a
fromSets = foldr joins mempty
 where joins set | (p0:ps) <- Set.toList set
                    = foldr ((.) . joinElems p0) id ps
       joins _empty = id

joinElems :: (Ord a) => a -> a -> DisjointSets a -> DisjointSets a
joinElems x y p@(DisjointSets pmap) =
    foldr1 (.) [ Map.insert o joined | o <- Set.toList joined] pmap
    & DisjointSets
    where
        joined = find x pmap `Set.union` find y pmap

find x pmap = fromMaybe (Set.singleton x) (Map.lookup x pmap)

insert x = joinElems x x

elems :: Ord a => DisjointSets a -> (Set.Set (Set.Set a))
elems (DisjointSets m) = Set.fromList (Map.elems m)

instance (Ord a) => Semigroup (DisjointSets a) where
    (DisjointSets a) <> (DisjointSets b) = fromSets $ Set.fromList (Map.elems a ++ Map.elems b)