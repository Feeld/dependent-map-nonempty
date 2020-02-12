{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.NonEmpty.Dependent.Map
    ( NonEmptyDMap
    , DSum(..), Some(..)
    , GCompare(..), GOrdering(..)

    -- * Operators
    , (\\)

    -- * Query
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

    , mapMaybe
    , mapMaybeWithKey

    -- * Submap
    , isSubmapOf, isSubmapOfBy
    , isProperSubmapOf, isProperSubmapOfBy

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

import           Prelude                hiding (lookup, map, null)

import           Data.Constraint.Extras
import           Data.Dependent.Map     (DMap)
import qualified Data.Dependent.Map     as D
import           Data.Dependent.Sum
import           Data.GADT.Compare
import           Data.GADT.Show
import           Data.List              (foldl')
import           Data.List.NonEmpty     (NonEmpty ((:|)))
import qualified Data.List.NonEmpty     as NE
import           Data.Maybe             (fromMaybe, isJust)
import           Data.Some
import           Text.Read

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
size (NonEmptyDMap _ xs) = D.size xs + 1

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as @('Just' value)@,
-- or 'Nothing' if the key isn't in the map.
lookup :: forall k f v. GCompare k => k v -> NonEmptyDMap k f -> Maybe (f v)
lookup x (NonEmptyDMap (k:=>v) xs)
  | Just Refl <- x `geq`  k = Just v
  | otherwise = D.lookup x xs


{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}

-- | Same as 'difference'.
(\\) :: GCompare k => NonEmptyDMap k f -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(log n)/. Is the key a member of the map? See also 'notMember'.
member :: GCompare k => k a -> NonEmptyDMap k f -> Bool
member k = isJust . lookup k

-- | /O(log n)/. Is the key not a member of the map? See also 'member'.
notMember :: GCompare k => k v -> NonEmptyDMap k f -> Bool
notMember k m = not (member k m)

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns default value @def@
-- when the key is not in the map.
findWithDefault :: GCompare k => f v -> k v -> NonEmptyDMap k f -> f v
findWithDefault def k = fromMaybe def . lookup k

{--------------------------------------------------------------------
  Insertion
--------------------------------------------------------------------}

-- | /O(log n)/. Insert a new key and value in the map.
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value. 'insert' is equivalent to
-- @'insertWith' 'const'@.
insert :: forall k f v. GCompare k => k v -> f v -> NonEmptyDMap k f -> NonEmptyDMap k f
insert !k v (NonEmptyDMap (k0:=>v0) xs) =
  case gcompare k k0 of
    GLT -> NonEmptyDMap (k:=>v) (D.insert k0 v0 xs)
    GEQ -> NonEmptyDMap (k:=>v) xs
    GGT -> NonEmptyDMap (k0:=>v0) (D.insert k v xs)


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
insertWithKey f k v = unsafeFromDMap . D.insertWithKey f k v . toDMap

-- | Same as 'insertWithKey', but the combining function is applied strictly.
insertWithKey' :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f -> NonEmptyDMap k f
insertWithKey' f k v = unsafeFromDMap . D.insertWithKey' f k v . toDMap

-- | /O(log n)/. Combines insert operation with old value retrieval.
-- The expression (@'insertLookupWithKey' f k x map@)
-- is a pair where the first element is equal to (@'lookup' k map@)
-- and the second element equal to (@'insertWithKey' f k x map@).
insertLookupWithKey :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f
                    -> (Maybe (f v), NonEmptyDMap k f)
insertLookupWithKey f k v m =
  let (x,m') = D.insertLookupWithKey f k v (toDMap m)
  in (x,unsafeFromDMap m')

-- | /O(log n)/. A strict version of 'insertLookupWithKey'.
insertLookupWithKey' :: forall k f v. GCompare k => (k v -> f v -> f v -> f v) -> k v -> f v -> NonEmptyDMap k f
                     -> (Maybe (f v), NonEmptyDMap k f)
insertLookupWithKey' f k v m =
  let (!x,!m') = D.insertLookupWithKey' f k v (toDMap m)
  in (x,unsafeFromDMap m')

{--------------------------------------------------------------------
  Deletion
  [delete] is the inlined version of [deleteWith (\k x -> Nothing)]
--------------------------------------------------------------------}

-- | /O(log n)/. Delete a key and its value from the map. When the key is not
-- a member of the map, the original map is returned.
delete :: forall k f v. GCompare k => k v -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
delete k = liftDM (D.delete k)

-- | /O(log n)/. Update a value at a specific key with the result of the provided function.
-- When the key is not
-- a member of the map, the original map is returned.
adjust :: GCompare k => (f v -> f v) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
adjust f = adjustWithKey (\_ x -> f x)

-- | /O(log n)/. Adjust a value at a specific key. When the key is not
-- a member of the map, the original map is returned.
adjustWithKey :: GCompare k => (k v -> f v -> f v) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
adjustWithKey f k = unsafeLiftDM (D.adjustWithKey f k)

-- | /O(log n)/. A strict version of 'adjustWithKey'.
adjustWithKey' :: GCompare k => (k v -> f v -> f v) -> k v -> NonEmptyDMap k f -> NonEmptyDMap k f
adjustWithKey' f !k = unsafeLiftDM (D.adjustWithKey' f k)

-- | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
-- at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
-- deleted. If it is (@'Just' y@), the key @k@ is bound to the new value @y@.
update :: GCompare k => (f v -> Maybe (f v)) -> k v -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
update f = updateWithKey (\_ x -> f x)

-- | /O(log n)/. The expression (@'updateWithKey' f k map@) updates the
-- value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
-- the element is deleted. If it is (@'Just' y@), the key @k@ is bound
-- to the new value @y@.
updateWithKey :: forall k f v. GCompare k => (k v -> f v -> Maybe (f v)) -> k v -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
updateWithKey f k = liftDM (D.updateWithKey f k)

-- | /O(log n)/. Lookup and update. See also 'updateWithKey'.
-- The function returns changed value, if it is updated.
-- Returns the original key value if the map entry is deleted.
updateLookupWithKey :: forall k f v. GCompare k => (k v -> f v -> Maybe (f v)) -> k v -> NonEmptyDMap k f -> (Maybe (f v), Maybe (NonEmptyDMap k f))
updateLookupWithKey f k m =
  let (x, m') = D.updateLookupWithKey f k (toDMap m)
  in (x, fromDMap m')

-- | /O(log n)/. The expression (@'alter' f k map@) alters the value @x@ at @k@, or absence thereof.
-- 'alter' can be used to insert, delete, or update a value in a 'Map'.
-- In short : @'lookup' k ('alter' f k m) = f ('lookup' k m)@.
alter :: forall k f v. GCompare k => (Maybe (f v) -> Maybe (f v)) -> k v -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
alter f k = liftDM (D.alter f k)

-- | Works the same as 'alter' except the new value is return in some 'Functor' @f@.
-- In short : @(\v' -> alter (const v') k dm) <$> f (lookup k dm)@
alterF :: forall k f v g. (GCompare  k, Functor f) => k v -> (Maybe (g v) -> f (Maybe (g v))) -> NonEmptyDMap k g -> f (Maybe (NonEmptyDMap k g))
alterF f k = fmap fromDMap . D.alterF f k . toDMap

{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}

-- | /O(1)/. The minimal key of the map.
findMin :: NonEmptyDMap k f -> DSum k f
findMin = lookupMin

lookupMin :: NonEmptyDMap k f -> DSum k f
lookupMin (NonEmptyDMap x _) = x

-- | /O(log n)/. The maximal key of the map.
findMax :: NonEmptyDMap k f -> DSum k f
findMax = lookupMax

lookupMax :: NonEmptyDMap k f -> DSum k f
lookupMax (NonEmptyDMap x xs) = fromMaybe x (D.lookupMax xs)

-- | /O(log n)/. Delete the minimal key.
deleteMin :: NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
deleteMin (NonEmptyDMap _ xs) = fromDMap xs

-- | /O(log n)/. Delete the maximal key.
deleteMax :: NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
deleteMax (NonEmptyDMap x xs)
  | D.null xs = Nothing
  | otherwise = Just (NonEmptyDMap x (D.deleteMax xs))

-- | /O(log n)/. Update the value at the minimal key.
updateMinWithKey :: (forall v. k v -> f v -> Maybe (f v)) -> NonEmptyDMap k f -> Maybe (NonEmptyDMap k f)
updateMinWithKey f (NonEmptyDMap (k :=> v) xs) =
  case f k v of
    Just v' -> Just (NonEmptyDMap (k :=> v') xs)
    Nothing -> Nothing

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
union a = fromMaybe (error "unreachable: union cannot produce empty maps") . liftDM2 D.union a


{--------------------------------------------------------------------
  Union with a combining function
--------------------------------------------------------------------}

-- | /O(n+m)/.
-- Union with a combining function.
unionWithKey :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> NonEmptyDMap k f -> NonEmptyDMap k f -> NonEmptyDMap k f
unionWithKey f a = fromMaybe (error "unreachable: union can't produce empty maps") . liftDM2 (D.unionWithKey f) a

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
isSubmapOf = isSubmapOfBy (\k _ x0 x1 -> has' @Eq @f k (x0 == x1))

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
isProperSubmapOf = isProperSubmapOfBy (\k _ x0 x1 -> has' @Eq @f k (x0 == x1))

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
mapAccumLWithKey
  :: forall k f g a. GCompare k
  => (forall v. a -> k v -> f v -> (a, g v)) -> a -> NonEmptyDMap k f -> (a, NonEmptyDMap k g)
mapAccumLWithKey f z m =
  let (a,m') = D.mapAccumLWithKey f z (toDMap m)
  in (a, unsafeFromDMap m')

-- | /O(n)/. The function 'mapAccumRWithKey' threads an accumulating
-- argument through the map in descending order of keys.
mapAccumRWithKey
  :: forall k f g a. GCompare k
  => (forall v. a -> k v -> f v -> (a, g v)) -> a -> NonEmptyDMap k f -> (a, NonEmptyDMap k g)
mapAccumRWithKey f z m =
  let (a,m') = D.mapAccumRWithKey f z (toDMap m)
  in (a, unsafeFromDMap m')

-- | /O(n*log n)/.
-- @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key.  In this case the associated values will be
-- combined using @c@.
mapKeysWith
  :: (GCompare k1, GCompare k2)
  => (forall v. k2 v -> f v -> f v -> f v) -> (forall v. k1 v -> k2 v) -> NonEmptyDMap k1 f -> NonEmptyDMap k2 f
mapKeysWith f g = unsafeLiftDM (D.mapKeysWith f g)


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
mapKeysMonotonic :: GCompare k1 => (forall v. k1 v -> k2 v) -> NonEmptyDMap k1 f -> NonEmptyDMap k2 f
mapKeysMonotonic f = unsafeLiftDM (D.mapKeysMonotonic f)

{--------------------------------------------------------------------
  Folds
--------------------------------------------------------------------}

-- | /O(n)/. Post-order fold.  The function will be applied from the lowest
-- value to the highest.
foldrWithKey :: GCompare k => (forall v. k v -> f v -> b -> b) -> b -> NonEmptyDMap k f -> b
foldrWithKey f z = D.foldrWithKey f z . toDMap

-- | /O(n)/. Pre-order fold.  The function will be applied from the highest
-- value to the lowest.
foldlWithKey :: GCompare k => (forall v. b -> k v -> f v -> b) -> b -> NonEmptyDMap k f -> b
foldlWithKey f z = D.foldlWithKey f z . toDMap

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}

-- | /O(n)/. Return all keys of the map in ascending order.
--
-- > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
-- > keys (singleton a) == a:|[]

keys  :: NonEmptyDMap k f -> NonEmpty (Some k)
keys (NonEmptyDMap (k:=>_) xs ) = Some k :| D.keys xs

-- | /O(n)/. Return all key\/value pairs in the map in ascending key order.
assocs :: NonEmptyDMap k f -> NonEmpty (DSum k f)
assocs (NonEmptyDMap x xs) = x :| D.assocs xs

{--------------------------------------------------------------------
  Lists
  use [foldlStrict] to reduce demand on the control-stack
--------------------------------------------------------------------}

-- | /O(n*log n)/. Build a map from a list of key\/value pairs. See also 'fromAscList'.
-- If the list contains more than one value for the same key, the last value
-- for the key is retained.
fromList :: GCompare k => NonEmpty (DSum k f) -> NonEmptyDMap k f
fromList = unsafeFromDMap . D.fromList . NE.toList

-- | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey'.
fromListWithKey :: GCompare k => (forall v. k v -> f v -> f v -> f v) -> NonEmpty (DSum k f) -> NonEmptyDMap k f
fromListWithKey f = unsafeFromDMap . D.fromListWithKey f . NE.toList

-- | /O(n)/. Convert to a list of key\/value pairs.
toList :: NonEmptyDMap k f -> NonEmpty (DSum k f)
toList = toAscList

-- | /O(n)/. Convert to an ascending list.
toAscList :: NonEmptyDMap k f -> NonEmpty (DSum k f)
toAscList = assocs

-- | /O(n)/. Convert to a descending list.
toDescList :: NonEmptyDMap k f -> NonEmpty (DSum k f)
toDescList (NonEmptyDMap x xs) = NE.fromList (D.toDescList xs <> [x])

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs       == fromList xs
    fromAscListWith f xs == fromListWith f xs
--------------------------------------------------------------------}

-- | /O(n)/. Build a map from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: GEq k => NonEmpty (DSum k f) -> NonEmptyDMap k f
fromAscList = fromAscListWithKey (\_ x _ -> x)

-- | /O(n)/. Build a map from an ascending list in linear time with a
-- combining function for equal keys.
-- /The precondition (input list is ascending) is not checked./
fromAscListWithKey :: GEq k => (forall v. k v -> f v -> f v -> f v) -> NonEmpty (DSum k f) -> NonEmptyDMap k f
fromAscListWithKey f = unsafeFromDMap . D.fromAscListWithKey f . NE.toList


-- | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
-- /The precondition is not checked./
fromDistinctAscList :: NonEmpty (DSum k f) -> NonEmptyDMap k f
fromDistinctAscList = unsafeFromDMap . D.fromDistinctAscList . NE.toList
--
-- | /O(log n)/. Retrieves the minimal (key :=> value) entry of the map, and
-- the map stripped of that element.
minViewWithKey, deleteFindMin :: forall k f . NonEmptyDMap k f -> (DSum k f, Maybe (NonEmptyDMap k f))
minViewWithKey (NonEmptyDMap x xs) = (x, fromDMap xs)
deleteFindMin = minViewWithKey

-- | /O(log n)/. Retrieves the maximal (key :=> value) entry of the map, and
-- the map stripped of that element.
maxViewWithKey, deleteFindMax :: forall k f . NonEmptyDMap k f -> (DSum k f, Maybe (NonEmptyDMap k f))
maxViewWithKey (NonEmptyDMap x xs) =
  case D.maxViewWithKey xs of
    Just (x', m') -> (x',fromDMap m')
    Nothing       -> (x, Nothing)
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

instance (GCompare k, GRead k, Has' Read k f) => Read (NonEmptyDMap k f) where
  readPrec = parens $ prec 10 $ do
    Ident "NonEmptyDMap" <- lexP
    NonEmptyDMap <$> readPrec <*> readPrec

  readListPrec = readListPrecDefault

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance (GShow k, Has' Show k f) => Show (NonEmptyDMap k f) where
    showsPrec p (NonEmptyDMap x xs) = showParen (p>10)
        ( showString "NonEmptyDMap "
        . showsPrec 11 x
        . showsPrec 11 xs
        )


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
liftDM
  :: forall k1 k2 f g. GCompare k1
  => (DMap k1 f -> DMap k2 g) -> NonEmptyDMap k1 f -> Maybe (NonEmptyDMap k2 g)
liftDM f = fromDMap . f . toDMap
{-# INLINE liftDM #-}

unsafeLiftDM
  :: forall k1 k2 f g. GCompare k1
  => (DMap k1 f -> DMap k2 g) -> NonEmptyDMap k1 f -> NonEmptyDMap k2 g
unsafeLiftDM f = unsafeFromDMap . f . toDMap
{-# INLINE unsafeLiftDM #-}

liftDM2
  :: forall k f g h. GCompare k
  => (DMap k f -> DMap k g -> DMap k h) -> NonEmptyDMap k f -> NonEmptyDMap k g -> Maybe (NonEmptyDMap k h)
liftDM2 f a b = fromDMap (f (toDMap a) (toDMap b))
{-# INLINE liftDM2 #-}

toDMap :: forall k f. GCompare k => NonEmptyDMap k f -> DMap k f
toDMap (NonEmptyDMap (k:=>v) xs) = D.insert k v xs
{-# INLINE toDMap #-}

fromDMap :: DMap k f -> Maybe (NonEmptyDMap k f)
fromDMap = fmap (uncurry NonEmptyDMap) . D.minViewWithKey
{-# INLINE fromDMap #-}

unsafeFromDMap :: DMap k f -> NonEmptyDMap k f
unsafeFromDMap = uncurry NonEmptyDMap . D.deleteFindMin
{-# INLINE unsafeFromDMap #-}
