{-# LANGUAGE GADTs, RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.NonEmpty.Dependent.Map
    ( NonEmptyDMap
    , DSum(..), Some(..)
    , GCompare(..), GOrdering(..)

    -- * Operators
    , (!), (\\)

    -- * Query
    , null
    , size
    , member
    , notMember
    , lookup
    , findWithDefault

    -- * Construction
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWith'
    , insertWithKey
    , insertWithKey'
    , insertLookupWithKey
    , insertLookupWithKey'

    -- ** Delete\/Update
    , delete
    , adjust
    , adjustWithKey
    , adjustWithKey'
    , update
    , updateWithKey
    , updateLookupWithKey
    , alter
    , alterF

    -- * Combine

    -- ** Union
    , union
    , unionWithKey
    , unions
    , unionsWithKey

    -- ** Difference
    , difference
    , differenceWithKey

    -- ** Intersection
    , intersection
    , intersectionWithKey

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , traverseWithKey
    , mapAccumLWithKey
    , mapAccumRWithKey
    , mapKeysWith
    , mapKeysMonotonic

    -- ** Fold
    , foldWithKey
    , foldrWithKey
    , foldlWithKey
    -- , foldlWithKey'

    -- * Conversion
    , keys
    , assocs

    -- ** Lists
    , toList
    , fromList
    , fromListWithKey

    -- ** Ordered lists
    , toAscList
    , toDescList
    , fromAscList
    , fromAscListWithKey
    , fromDistinctAscList

    -- * Filter
    , filter
    , filterWithKey
    , partitionWithKey

    , mapMaybe
    , mapMaybeWithKey
    , mapEitherWithKey

    , split
    , splitLookup

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

    -- * Indexed
    , lookupIndex
    , findIndex
    , elemAt
    , updateAt
    , deleteAt

    -- * Min\/Max
    , findMin
    , findMax
    , lookupMin
    , lookupMax
    , deleteMin
    , deleteMax
    , deleteFindMin
    , deleteFindMax
    , updateMinWithKey
    , updateMaxWithKey
    , minViewWithKey
    , maxViewWithKey

    ) where

import Prelude hiding (null, lookup, map)
import Control.Arrow ((***))
import qualified Prelude

import Data.Dependent.Sum
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as D
import Data.Constraint.Extras
import Data.GADT.Compare
import Data.GADT.Show
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, fromMaybe)
import Data.List (foldl')
import Data.Semigroup
import Data.Some
import Text.Read

data NonEmptyDMap k f = NonEmptyDMap (DSum k f) (D.DMap k f)

instance (GCompare k) => Semigroup (NonEmptyDMap k f) where
  (<>) = union

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}

-- | /O(1)/. A map with a single element.
--
-- > singleton 1 'a'        == fromList [(1, 'a')]
-- > size (singleton 1 'a') == 1
singleton :: k v -> f v -> NonEmptyDMap k f
singleton k v = NonEmptyDMap (k :=> v) D.empty 

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. The number of elements in the map.
size :: NonEmptyDMap k f -> Int
size = error "not implemented"
{-
size Tip                = 0
size (Bin n _ _ _ _)    = n
-}

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
lookup :: forall k f v. GCompare k => k v -> NonEmptyDMap k f -> Maybe (f v)
lookup = error "not implemented"
{-
lookup k = k `seq` go
    where
        go :: NonEmptyDMap k f -> Maybe (f v)
        go Tip = Nothing
        go (Bin _ kx x l r) = 
            case gcompare k kx of
                GLT -> go l
                GGT -> go r
                GEQ -> Just x
                -}

lookupAssoc :: forall k f v. GCompare k => Some k -> NonEmptyDMap k f -> Maybe (DSum k f)
lookupAssoc = error "not implemented"
{-
lookupAssoc (This k) = k `seq` go
  where
    go :: NonEmptyDMap k f -> Maybe (DSum k f)
    go Tip = Nothing
    go (Bin _ kx x l r) =
        case gcompare k kx of
            GLT -> go l
            GGT -> go r
            GEQ -> Just (kx :=> x)
            -}

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 !,\\ --

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
--
-- > fromList [(5,'a'), (3,'b')] ! 1    Error: element not in the map
-- > fromList [(5,'a'), (3,'b')] ! 5 == 'a'

(!) :: GCompare k => NonEmptyDMap k f -> k v -> f v
(!) m k    = find k m

-- | Same as 'difference'.
(\\) :: GCompare k => NonEmptyDMap k f -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
m1 \\ m2 = difference m1 m2

-- #if __GLASGOW_HASKELL__
--
-- {--------------------------------------------------------------------
--   A Data instance
-- --------------------------------------------------------------------}
--
-- -- This instance preserves data abstraction at the cost of inefficiency.
-- -- We omit reflection services for the sake of data abstraction.
--
-- instance (Data k, Data a, GCompare k) => Data (NonEmptyDMap k) where
--   gfoldl f z m   = z fromList `f` toList m
--   toConstr _     = error "toConstr"
--   gunfold _ _    = error "gunfold"
--   dataTypeOf _   = mkNoRepType "Data.Map.Map"
--   dataCast2 f    = gcast2 f
--
-- #endif

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: GCompare k => k a -> NonEmptyDMap k f -> Bool
member k = isJust . lookup k

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
notMember :: GCompare k => k v -> NonEmptyDMap k f -> Bool
notMember k m = not (member k m)

-- | /O(log n)/. Find the value at a key.
-- Calls 'error' when the element can not be found.
-- Consider using 'lookup' when elements may not be present.
find :: GCompare k => k v -> NonEmptyDMap k f -> f v
find k m = case lookup k m of
    Nothing -> error "NonEmptyDMap.find: element not in the map"
    Just v  -> v

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
findWithDefault :: GCompare k => f v -> k v -> NonEmptyDMap k f -> f v
findWithDefault def k m = case lookup k m of
    Nothing -> def
    Just v  -> v

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: forall k f v. GCompare k => k v -> f v -> NonEmptyDMap k f -> NonEmptyDMap k f
insert = error "not implemented"
{-
insert kx x = kx `seq` go
    where
        go :: NonEmptyDMap k f -> NonEmptyDMap k f
        go Tip = singleton kx x
        go t@(Bin sz ky y l r) = case gcompare kx ky of
            GLT -> let !l' = go l
                   in if l' `ptrEq` l
                      then t
                      else balance ky y l' r
            GGT -> let !r' = go r
                   in if r' `ptrEq` r
                      then t
                      else balance ky y l r'
            GEQ
              | kx `ptrEq` ky && x `ptrEq` y -> t
              | otherwise -> Bin sz kx x l r
              -}

-- | /O(log n)/. Insert a new key and value in the map if the key
-- is not already present. If the key is already present, @insertR@
-- does nothing.
insertR :: forall k f v. GCompare k => k v -> f v -> NonEmptyDMap k f -> NonEmptyDMap k f
insertR = error "not implemented"
{-
insertR kx x = kx `seq` go
    where
        go :: NonEmptyDMap k f -> NonEmptyDMap k f
        go Tip = singleton kx x
        go t@(Bin sz ky y l r) = case gcompare kx ky of
            GLT -> let !l' = go l
                   in if l' `ptrEq` l
                      then t
                      else balance ky y l' r
            GGT -> let !r' = go r
                   in if r' `ptrEq` r
                   then t
                   else balance ky y l r'
            GEQ -> t
            -}

-- | /O(log n)/. Insert with a function, combining new value and old value.
-- @'insertWith' f key value mp@
-- will insert the entry @key :=> value@ into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the entry @key :=> f new_value old_value@.
insertWith :: GCompare k => (f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f -> NonEmptyDMap k f
insertWith f = insertWithKey (\_ x' y' -> f x' y')

-- | Same as 'insertWith', but the combining function is applied strictly.
-- This is often the most desirable behavior.
insertWith' :: GCompare k => (f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f -> NonEmptyDMap k f
insertWith' f = insertWithKey' (\_ x' y' -> f x' y')

-- | /O(log n)/. Insert with a function, combining key, new value and old value.
-- @'insertWithKey' f key value mp@
-- will insert the entry @key :=> value@ into @mp@ if key does
-- not exist in the map. If the key does exist, the function will
-- insert the entry @key :=> f key new_value old_value@.
-- Note that the key passed to f is the same key passed to 'insertWithKey'.
insertWithKey :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f -> NonEmptyDMap k f
insertWithKey = error "not implemented"
{-
insertWithKey f kx x = kx `seq` go
  where
    go :: NonEmptyDMap k f -> NonEmptyDMap k f
    go Tip = singleton kx x
    go (Bin sy ky y l r) =
        case gcompare kx ky of
            GLT -> balance ky y (go l) r
            GGT -> balance ky y l (go r)
            GEQ -> Bin sy kx (f kx x y) l r
            -}

-- | Same as 'insertWithKey', but the combining function is applied strictly.
insertWithKey' :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f -> NonEmptyDMap k f
insertWithKey' = error "not implemented"
{-
insertWithKey' f kx x = kx `seq` go
  where
    go :: NonEmptyDMap k f -> NonEmptyDMap k f
    go Tip = singleton kx $! x
    go (Bin sy ky y l r) =
        case gcompare kx ky of
            GLT -> balance ky y (go l) r
            GGT -> balance ky y l (go r)
            GEQ -> let x' = f kx x y in seq x' (Bin sy kx x' l r)
            -}

-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
insertLookupWithKey :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f
                    -> (Maybe (f v), NonEmptyDMap k f)
insertLookupWithKey = error "not implemented"
                    {-
insertLookupWithKey f kx x = kx `seq` go
  where
    go :: NonEmptyDMap k f -> (Maybe (f v), NonEmptyDMap k f)
    go Tip = (Nothing, singleton kx x)
    go (Bin sy ky y l r) =
        case gcompare kx ky of
            GLT -> let (found, l') = go l
                  in (found, balance ky y l' r)
            GGT -> let (found, r') = go r
                  in (found, balance ky y l r')
            GEQ -> (Just y, Bin sy kx (f kx x y) l r)
            -}

-- | /O(log n)/. A strict version of 'insertLookupWithKey'.
insertLookupWithKey' :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f
                     -> (Maybe (f v), NonEmptyDMap k f)
insertLookupWithKey' = error "not implemented"
{-
insertLookupWithKey' f kx x = kx `seq` go
  where
    go :: NonEmptyDMap k f -> (Maybe (f v), NonEmptyDMap k f)
    go Tip = x `seq` (Nothing, singleton kx x)
    go (Bin sy ky y l r) =
        case gcompare kx ky of
            GLT -> let (found, l') = go l
                  in (found, balance ky y l' r)
            GGT -> let (found, r') = go r
                  in (found, balance ky y l r')
            GEQ -> let x' = f kx x y in x' `seq` (Just y, Bin sy kx x' l r)
            -}

{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: forall k f v. GCompare k => k v -> NonEmptyDMap k f -> NonEmptyDMap k f
delete = error "not implemented"
{-
delete k = k `seq` go
  where
    go :: NonEmptyDMap k f -> NonEmptyDMap k f
    go Tip = Tip
    go (Bin _ kx x l r) =
        case gcompare k kx of
            GLT -> balance kx x (go l) r
            GGT -> balance kx x l (go r)
            GEQ -> glue l r
            -}

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
adjust :: GCompare k => (f v -> f v) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
adjust f = adjustWithKey (\_ x -> f x)

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjustWithKey :: GCompare k => (k v -> f v -> f v) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
adjustWithKey = error "not implemented"
{-
adjustWithKey f0 !k0 = go f0 k0
  where
    go :: GCompare k => (k v -> f v -> f v) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
    go _f _k Tip = Tip
    go f k (Bin sx kx x l r) =
      case gcompare k kx of
        GLT -> Bin sx kx x (go f k l) r
        GGT -> Bin sx kx x l (go f k r)
        GEQ -> Bin sx kx (f kx x) l r
        -}

-- | /O(log n)/. A strict version of 'adjustWithKey'.
adjustWithKey' :: GCompare k => (k v -> f v -> f v) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
adjustWithKey' = error "not implemented"
{-
adjustWithKey' f0 !k0 = go f0 k0
  where
    go :: GCompare k => (k v -> f v -> f v) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
    go _f _k Tip = Tip
    go f k (Bin sx kx x l r) =
      case gcompare k kx of
        GLT -> Bin sx kx x (go f k l) r
        GGT -> Bin sx kx x l (go f k r)
        GEQ -> let !x' = f kx x in Bin sx kx x' l r
        -}

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: GCompare k => (f v -> Maybe (f v)) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
update f = updateWithKey (\_ x -> f x)

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
updateWithKey :: forall k f v. GCompare k => (k v -> f v -> Maybe (f v)) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
updateWithKey = error "not implemented"
{-
updateWithKey f k = k `seq` go
  where
    go :: NonEmptyDMap k f -> NonEmptyDMap k f
    go Tip = Tip
    go (Bin sx kx x l r) =
        case gcompare k kx of
           GLT -> balance kx x (go l) r
           GGT -> balance kx x l (go r)
           GEQ -> case f kx x of
                   Just x' -> Bin sx kx x' l r
                   Nothing -> glue l r
                   -}

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
updateLookupWithKey :: forall k f v. GCompare k => (k v -> f v -> Maybe (f v)) -> k v -> NonEmptyDMap k f -> (Maybe (f v), NonEmptyDMap k f)
updateLookupWithKey = error "not implemented"
{-
updateLookupWithKey f k = k `seq` go
 where
   go :: NonEmptyDMap k f -> (Maybe (f v), NonEmptyDMap k f)
   go Tip = (Nothing,Tip)
   go (Bin sx kx x l r) =
          case gcompare k kx of
               GLT -> let (found,l') = go l in (found,balance kx x l' r)
               GGT -> let (found,r') = go r in (found,balance kx x l r')
               GEQ -> case f kx x of
                       Just x' -> (Just x',Bin sx kx x' l r)
                       Nothing -> (Just x,glue l r)
                       -}

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: forall k f v. GCompare k => (Maybe (f v) -> Maybe (f v)) -> k v -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
alter = error "not implemented"
{-
alter f k = k `seq` go
  where
    go :: NonEmptyDMap k f -> NonEmptyDMap k f
    go Tip = case f Nothing of
               Nothing -> Tip
               Just x  -> singleton k x

    go (Bin sx kx x l r) = case gcompare k kx of
               GLT -> balance kx x (go l) r
               GGT -> balance kx x l (go r)
               GEQ -> case f (Just x) of
                       Just x' -> Bin sx kx x' l r
                       Nothing -> glue l r
                       -}

-- | Works the same as 'alter' except the new value is return in some 'Functor' @f@.
-- In short : @(\v' -> alter (const v') k dm) <$> f (lookup k dm)@
alterF :: forall k f v g. (GCompare  k, Functor f) => k v -> (Maybe (g v) -> f (Maybe (g v))) -> NonEmptyDMap k g -> f (NonEmptyDMap k g)
alterF = error "not implemented"
{-
alterF k f = go
  where
    go :: NonEmptyDMap k g -> f (NonEmptyDMap k g)
    go Tip = maybe Tip (singleton k) <$> f Nothing

    go (Bin sx kx x l r) = case gcompare k kx of
      GLT -> (\l' -> balance kx x l' r) <$> go l
      GGT -> (\r' -> balance kx x l r') <$> go r
      GEQ -> maybe (glue l r) (\x' -> Bin sx kx x' l r) <$> f (Just x)
      -}

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(log n)/. The minimal key of the map.
findMin :: NonEmptyDMap k f -> DSum k f
findMin = lookupMin

lookupMin :: NonEmptyDMap k f -> DSum k f
lookupMin = error "not implemented"
{-
lookupMin m = case m of
      Tip -> Nothing
      Bin _ kx x l _ -> Just $! go kx x l
  where
    go :: k v -> f v -> NonEmptyDMap k f -> DSum k f
    go kx x Tip = kx :=> x
    go _  _ (Bin _ kx x l _) = go kx x l
    -}

-- | /O(log n)/. The maximal key of the map.
findMax :: NonEmptyDMap k f -> DSum k f
findMax = lookupMax

lookupMax :: NonEmptyDMap k f -> DSum k f
lookupMax = error "not implemented"
{-
lookupMax m = case m of
      Tip -> Nothing
      Bin _ kx x _ r -> Just $! go kx x r
  where
    go :: k v -> f v -> NonEmptyDMap k f -> DSum k f
    go kx x Tip = kx :=> x
    go _  _ (Bin _ kx x _ r) = go kx x r
    -}

-- | /O(log n)/. Delete the minimal key.
deleteMin :: NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
deleteMin = error "not implemented"

-- | /O(log n)/. Delete the maximal key.
deleteMax :: NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
deleteMax = error "not implemented"

-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: (forall v. k v -> f v -> Maybe (f v)) -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
updateMinWithKey f (NonEmptyDMap (k :=> v) xs) = 
  case f k v of
    Just v' -> Just (NonEmptyDMap (k :=> v') xs)
    Nothing  -> Nothing

-- | /O(log n)/. Update the value at the maximal key.
updateMaxWithKey :: (forall v. k v -> f v -> Maybe (f v)) -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
updateMaxWithKey f (NonEmptyDMap (k:=>v) xs)
  | D.null xs =
      case f k v of
        Just v' -> Just (NonEmptyDMap (k:=>v') xs)
        Nothing -> Nothing
  | otherwise = Just (NonEmptyDMap (k:=>v) (D.updateMaxWithKey f xs))

{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}

-- | The union of a list of maps:
unions :: GCompare k => NonEmpty (NonEmptyDMap k f) -> NonEmptyDMap k f
unions (x:|xs) = foldl' union x xs

-- | The union of a list of maps, with a combining operation:
unionsWithKey :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> NonEmpty (NonEmptyDMap k f) -> NonEmptyDMap k f
unionsWithKey f (x:|xs) = foldl' (unionWithKey f) x xs

-- | /O(m*log(n\/m + 1)), m <= n/.
-- The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
-- It prefers @t1@ when duplicate keys are encountered,
-- i.e. (@'union' == 'unionWith' 'const'@).
union :: GCompare k => NonEmptyDMap k f -> NonEmptyDMap k f -> NonEmptyDMap k f
union = error "not implemented"
{-
union t1 Tip  = t1
union t1 (Bin _ kx x Tip Tip) = insertR kx x t1
union Tip t2  = t2
union (Bin _ kx x Tip Tip) t2 = insert kx x t2
union t1@(Bin _ k1 x1 l1 r1) t2 = case split k1 t2 of
  (l2, r2)
    | l1 `ptrEq` l1l2 && r1 `ptrEq` r1r2 -> t1
    | otherwise -> combine k1 x1 l1l2 r1r2
    where !l1l2 = l1 `union` l2
          !r1r2 = r1 `union` r2
          -}

liftDM
  :: forall k f g h. GCompare k
  => (DMap k f -> DMap k g) -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k g)
liftDM f = fromDMap . f . toDMap

unsafeLiftDM
  :: forall k f g h. GCompare k
  => (DMap k f -> DMap k g) -> NonEmptyDMap k f -> NonEmptyDMap k g
unsafeLiftDM f = unsafeFromDMap . f . toDMap

liftDM2
  :: forall k f g h. GCompare k
  => (DMap k f -> DMap k g -> DMap k h) -> NonEmptyDMap k f -> NonEmptyDMap k g -> Maybe (NonEmptyDMap k h)
liftDM2 f a b = fromDMap (f (toDMap a) (toDMap b))

toDMap :: forall k f. GCompare k => NonEmptyDMap k f -> DMap k f
toDMap (NonEmptyDMap (k:=>v) xs) = D.insert k v xs

fromDMap :: DMap k f -> Maybe (NonEmptyDMap k f)
fromDMap = fmap (uncurry NonEmptyDMap) . D.minViewWithKey

unsafeFromDMap :: DMap k f -> NonEmptyDMap k f
unsafeFromDMap = uncurry NonEmptyDMap . D.deleteFindMin

{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}

-- | /O(n+m)/.
-- Union with a combining function.
unionWithKey :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> NonEmptyDMap k f -> NonEmptyDMap k f -> NonEmptyDMap k f
unionWithKey f a = fromMaybe (error "union can't produce empty maps") . liftDM2 (D.unionWithKey f) a

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}

-- | /O(m * log (n\/m + 1)), m <= n/. Difference of two maps.
-- Return elements of the first map not existing in the second map.
difference :: GCompare k => NonEmptyDMap k f -> NonEmptyDMap k g -> Maybe (NonEmptyDMap k f)
difference = liftDM2 D.difference

-- | /O(n+m)/. Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the key and both values.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWithKey :: GCompare k => (forall v. k v -> f v -> g v -> Maybe (f v)) -> NonEmptyDMap k f -> NonEmptyDMap k g -> Maybe (NonEmptyDMap k f)
differenceWithKey f = liftDM2 (D.differenceWithKey f)

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}

-- | /O(m * log (n\/m + 1), m <= n/. Intersection of two maps.
-- Return data in the first map for the keys existing in both maps.
-- (@'intersection' m1 m2 == 'intersectionWith' 'const' m1 m2@).
intersection :: GCompare k => NonEmptyDMap k f -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
intersection = liftDM2 D.intersection

-- | /O(m * log (n\/m + 1), m <= n/. Intersection with a combining function.
intersectionWithKey :: GCompare k => (forall v. k v -> f v -> g v -> h v) -> NonEmptyDMap k f -> NonEmptyDMap k g -> Maybe (NonEmptyDMap k h)
intersectionWithKey f = liftDM2 (D.intersectionWithKey f)
{-
intersectionWithKey _ Tip _ = Tip
intersectionWithKey _ _ Tip = Tip
intersectionWithKey f (Bin s1 k1 x1 l1 r1) t2 =
  let !(l2, found, r2) = splitLookup k1 t2
      !l1l2 = intersectionWithKey f l1 l2
      !r1r2 = intersectionWithKey f r1 r2
  in case found of
       Nothing -> merge l1l2 r1r2
       Just x2 -> combine k1 (f k1 x1 x2) l1l2 r1r2
       -}

{--------------------------------------------------------------------
  Submap
--------------------------------------------------------------------}
-- | /O(n+m)/.
-- This function is defined as (@'isSubmapOf' = 'isSubmapOfBy' 'eqTagged')@).
--
isSubmapOf
  :: forall k f
  .  (GCompare k, Has' Eq k f)
  => NonEmptyDMap k f -> NonEmptyDMap k f -> Bool
isSubmapOf m1 m2 = isSubmapOfBy (\k _ x0 x1 -> has' @Eq @f k (x0 == x1)) m1 m2

{- | /O(n+m)/.
 The expression (@'isSubmapOfBy' f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective keys and values.
-}
isSubmapOfBy :: GCompare k => (forall v. k v -> k v -> f v -> g v -> Bool) -> NonEmptyDMap k f -> NonEmptyDMap k g -> Bool
isSubmapOfBy f t1 t2 = D.isSubmapOfBy f (toDMap t1) (toDMap t2)


-- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
-- Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy' 'eqTagged'@).
isProperSubmapOf
  :: forall k f
  .  (GCompare k, Has' Eq k f)
  => NonEmptyDMap k f -> NonEmptyDMap k f -> Bool
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (\k _ x0 x1 -> has' @Eq @f k (x0 == x1)) m1 m2

{- | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy' f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective keys and values.
-}
isProperSubmapOfBy :: GCompare k => (forall v. k v -> k v -> f v -> g v -> Bool) -> NonEmptyDMap k f -> NonEmptyDMap k g -> Bool
isProperSubmapOfBy f t1 t2
  = D.isProperSubmapOfBy f (toDMap t1) (toDMap t2)

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}

-- | /O(n)/. Filter all keys\/values that satisfy the predicate.
filterWithKey :: GCompare k => (forall v. k v -> f v -> Bool) -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
filterWithKey f = liftDM (D.filterWithKey f)


-- | /O(n)/. Map values and collect the 'Just' results.
mapMaybe :: GCompare k => (forall v. f v -> Maybe (g v)) -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k g)
mapMaybe f = mapMaybeWithKey (const f)

-- | /O(n)/. Map keys\/values and collect the 'Just' results.
mapMaybeWithKey :: GCompare k => (forall v. k v -> f v -> Maybe (g v)) -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k g)
mapMaybeWithKey f = liftDM (D.mapMaybeWithKey f)


{--------------------------------------------------------------------
  Mapping
--------------------------------------------------------------------}

-- | /O(n)/. Map a function over all values in the map.
map :: (forall v. f v -> g v) -> NonEmptyDMap k f -> NonEmptyDMap k g
map f (NonEmptyDMap (k:=>v) xs) = NonEmptyDMap (k:=>f v) (D.map f xs)

-- | /O(n)/. Map a function over all values in the map.
mapWithKey :: (forall v. k v -> f v -> g v) -> NonEmptyDMap k f -> NonEmptyDMap k g
mapWithKey f (NonEmptyDMap (k:=>v) xs) = NonEmptyDMap (k:=>f k v) (D.mapWithKey f xs)

-- | /O(n)/.
-- @'traverseWithKey' f m == 'fromList' <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
traverseWithKey :: Applicative t => (forall v. k v -> f v -> t (g v)) -> NonEmptyDMap k f -> t (NonEmptyDMap k g)
traverseWithKey f (NonEmptyDMap (k:=>v) xs) = NonEmptyDMap <$> ((k:=>) <$> f k v) <*> D.traverseWithKey f xs

-- | /O(n)/. The function 'mapAccumLWithKey' threads an accumulating
-- argument throught the map in ascending order of keys.
mapAccumLWithKey :: (forall v. a -> k v -> f v -> (a, g v)) -> a -> NonEmptyDMap k f -> (a, NonEmptyDMap k g)
mapAccumLWithKey = error "not implemented"
{-
mapAccumLWithKey f = go
  where
    go a Tip               = (a,Tip)
    go a (Bin sx kx x l r) =
                 let (a1,l') = go a l
                     (a2,x') = f a1 kx x
                     (a3,r') = go a2 r
                 in (a3,Bin sx kx x' l' r')
                 -}

-- | /O(n)/. The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey :: (forall v. a -> k v -> f v -> (a, g v)) -> a -> NonEmptyDMap k f -> (a, NonEmptyDMap k g)
mapAccumRWithKey = error "not implemented"
{-
mapAccumRWithKey f = go
  where
    go a Tip = (a,Tip)
    go a (Bin sx kx x l r) =
                 let (a1,r') = go a r
                     (a2,x') = f a1 kx x
                     (a3,l') = go a2 l
                 in (a3,Bin sx kx x' l' r')
                 -}

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
mapKeysWith :: GCompare k2 => (forall v. k2 v -> f v -> f v -> f v) -> (forall v. k1 v -> k2 v) -> NonEmptyDMap k1 f -> NonEmptyDMap k2 f
mapKeysWith = error "not implemented"
{-
mapKeysWith c f = fromListWithKey c . Prelude.map fFirst . toList
    where fFirst (x :=> y) = (f x :=> y)
    -}


-- | /O(n)/.
-- @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
-- is strictly monotonic.
-- That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapKeysMonotonic f s == mapKeys f s
-- >     where ls = keys s
--
-- This means that @f@ maps distinct original keys to distinct resulting keys.
-- This function has better performance than 'mapKeys'.
mapKeysMonotonic :: (forall v. k1 v -> k2 v) -> NonEmptyDMap k1 f -> NonEmptyDMap k2 f
mapKeysMonotonic = error "not implemented"
{-
mapKeysMonotonic _ Tip = Tip
mapKeysMonotonic f (Bin sz k x l r) =
    Bin sz (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)
    -}

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Post-order fold.  The function will be applied from the lowest
-- value to the highest.
foldrWithKey :: (forall v. k v -> f v -> b -> b) -> b -> NonEmptyDMap k f -> b
foldrWithKey = error "not implemented"
{-
foldrWithKey f = go
  where
    go z Tip              = z
    go z (Bin _ kx x l r) = go (f kx x (go z r)) l
    -}

-- | /O(n)/. Pre-order fold.  The function will be applied from the highest
-- value to the lowest.
foldlWithKey :: (forall v. b -> k v -> f v -> b) -> b -> NonEmptyDMap k f -> b
foldlWithKey = error "not implemented"
{-
foldlWithKey f = go
  where
    go z Tip              = z
    go z (Bin _ kx x l r) = go (f (go z l) kx x) r
    -}

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys (singleton a) == a:|[]

keys  :: NonEmptyDMap k f -> NonEmpty (Some k)
keys = error "not implemented"
  -- = [This k | (k :=> _) <- assocs m]

-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
assocs :: NonEmptyDMap k f -> NonEmpty (DSum k f)
assocs = error "not implemented"
  -- = toList m

{--------------------------------------------------------------------
  Lists
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
fromList :: GCompare k => NonEmpty (DSum k f) -> NonEmptyDMap k f
fromList = error "not implemented"
{-
fromList xs
  = foldlStrict ins empty xs
  where
    ins :: GCompare k => NonEmptyDMap k f -> DSum k f -> NonEmptyDMap k f
    ins t (k :=> x) = insert k x t
-}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
fromListWithKey :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> NonEmpty (DSum k f) -> NonEmptyDMap k f
fromListWithKey = error "not implemented"
{-
fromListWithKey f xs
  = foldlStrict (ins f) empty xs
  where
    ins :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> NonEmptyDMap k f -> DSum k f -> NonEmptyDMap k f
    ins f t (k :=> x) = insertWithKey f k x t
    -}

-- | /O(n)/. Convert to a list of key\/value pairs.
toList :: NonEmptyDMap k f -> NonEmpty (DSum k f)
toList t      = toAscList t

-- | /O(n)/. Convert to an ascending list.
toAscList :: NonEmptyDMap k f -> NonEmpty (DSum k f)
toAscList = error "not implemented"
{-
toAscList t   = foldrWithKey (\k x xs -> (k :=> x):xs) [] t
-}

-- | /O(n)/. Convert to a descending list.
toDescList :: NonEmptyDMap k f -> NonEmpty (DSum k f)
toDescList = error "not implemented"
{-
toDescList t  = foldlWithKey (\xs k x -> (k :=> x):xs) [] t
-}

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}

-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: GEq k => NonEmpty (DSum k f) -> NonEmptyDMap k f
fromAscList xs
  = fromAscListWithKey (\_ x _ -> x) xs

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: GEq k => (forall v. k v -> f v -> f v -> f v) -> NonEmpty (DSum k f) -> NonEmptyDMap k f
fromAscListWithKey = error "not implemented"
{-
fromAscListWithKey f xs
  = fromDistinctAscList (combineEq f xs)
  where
  -- [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq _ xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' f x xx

  combineEq' :: GEq k => (forall v. k v -> f v -> f v -> f v) -> DSum k f -> [DSum k f] -> [DSum k f]
  combineEq' f z [] = [z]
  combineEq' f z@(kz :=> zz) (x@(kx :=> xx):xs') =
    case geq kx kz of
        Just Refl   -> let yy = f kx xx zz in combineEq' f (kx :=> yy) xs'
        Nothing     -> z : combineEq' f x xs'
        -}


-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
fromDistinctAscList :: NonEmpty (DSum k f) -> NonEmptyDMap k f
fromDistinctAscList = error "not implemented"
{-
fromDistinctAscList xs
  = build const (length xs) xs
  where
    -- 1) use continutations so that we use heap space instead of stack space.
    -- 2) special case for n==5 to build bushier trees.

    build :: (NonEmptyDMap k f -> [DSum k f] -> b) -> Int -> [DSum k f] -> b
    build c 0 xs'  = c Tip xs'
    build c 5 xs'  = case xs' of
                       ((k1:=>x1):(k2:=>x2):(k3:=>x3):(k4:=>x4):(k5:=>x5):xx)
                            -> c (bin k4 x4 (bin k2 x2 (singleton k1 x1) (singleton k3 x3)) (singleton k5 x5)) xx
                       _ -> error "fromDistinctAscList build"
    build c n xs'  = seq nr $ build (buildR nr c) nl xs'
                   where
                     nl = n `div` 2
                     nr = n - nl - 1

    buildR :: Int -> (NonEmptyDMap k f -> [DSum k f] -> b) -> NonEmptyDMap k f -> [DSum k f] -> b
    buildR n c l ((k:=>x):ys) = build (buildB l k x c) n ys
    buildR _ _ _ []           = error "fromDistinctAscList buildR []"

    buildB :: NonEmptyDMap k f -> k v -> f v -> (NonEmptyDMap k f -> a -> b) -> NonEmptyDMap k f -> a -> b
    buildB l k x c r zs       = c (bin k x l r) zs
    -}
--
-- | /O(log n)/. Retrieves the minimal (key :=> value) entry of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
minViewWithKey, deleteFindMin :: forall k f . DMap k f -> (DSum k f, NonEmptyDMap k f)
minViewWithKey = error "not implemented"
deleteFindMin = minViewWithKey

-- | /O(log n)/. Retrieves the maximal (key :=> value) entry of the map, and
-- the map stripped of that element, or 'Nothing' if passed an empty map.
maxViewWithKey, deleteFindMax :: forall k f . DMap k f -> (DSum k f, DMap k f)
maxViewWithKey = error "not implemented"
deleteFindMax = maxViewWithKey


{--------------------------------------------------------------------
  Eq converts the tree to a list. In a lazy setting, this
  actually seems one of the faster methods to compare two trees
  and it is certainly the simplest :-)
--------------------------------------------------------------------}
instance (GEq k, Has' Eq k f) => Eq (NonEmptyDMap k f) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance (GCompare k, Has' Eq k f, Has' Ord k f) => Ord (NonEmptyDMap k f) where
  compare m1 m2 = compare (toAscList m1) (toAscList m2)

{--------------------------------------------------------------------
  Read
--------------------------------------------------------------------}

instance (GCompare k, GRead k, Has' Read k f) => Read (NonEmptyDMap k f) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)

  readListPrec = readListPrecDefault

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (GShow k, Has' Show k f) => Show (NonEmptyDMap k f) where
    showsPrec p m = showParen (p>10)
        ( showString "fromList "
        . showsPrec 11 (toList m)
        )

{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = z `seq` go (f z x) xs
