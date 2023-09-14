module Data.Trie
where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ( (<|>) )
import Data.Maybe (listToMaybe)

data Trie k v =
  Trie (Maybe v) (Map k (Trie k v))
  deriving (Show)

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup [] (Trie x _) = x
lookup (k:ks) (Trie _ m) =
  Map.lookup k m >>= lookup ks

lookupWith :: [k -> Bool] -> Trie k v -> Maybe v
lookupWith [] (Trie x _) = x
lookupWith (f:fs) (Trie _ m) =
  listToMaybe [ v | (k, v) <- Map.toList m, f k ] >>= lookupWith fs

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert [] v (Trie _ m) = Trie (Just v) m
insert (k:ks) v (Trie t m) =
  Trie t m'
  where
    m' = case Map.lookup k m of
          Nothing -> Map.insert k (insert ks v empty) m
          Just st -> Map.insert k (insert ks v st) m

union :: Ord k => Trie k v -> Trie k v -> Trie k v
union (Trie v1 m1) (Trie v2 m2) =
  Trie
    (v1 <|> v2)
    (Map.unionWith union m1 m2)

empty :: Ord k => Trie k v
empty = Trie Nothing mempty

instance Ord k => Semigroup (Trie k v) where
  (<>) = union

instance Ord k => Monoid (Trie k v) where
  mempty = empty
