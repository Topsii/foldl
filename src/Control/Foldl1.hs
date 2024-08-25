{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ImportQualifiedPost       #-}
{-# LANGUAGE LambdaCase                #-}

module Control.Foldl1 (
    -- * Fold1 Types
      Fold1(..)
    , FoldM1(..)

    -- * Folding
    , fold1
    , foldM1
    -- , scan
    -- , prescan
    -- , postscan

    -- * Conversion between Fold and Fold1 from Control.Foldl1
    , fromFold
    , fromFoldM
    , toFold

    -- * Conversion between Fold and Fold1 from Control.Foldl.NonEmpty
    , fromFoldNE
    , toFoldNE

    -- * Fold1s
    , Control.Foldl1.sconcat
    , Control.Foldl1.foldMap1
    , Control.Foldl1.foldl1
    , head
    , last
    , maximum
    , maximumBy
    , minimum
    , minimumBy
    , sink

    -- * Non-empty Container Fold1s
    , nonEmpty
    , revNonEmpty
    -- , nubNonEmpty
    -- , eqNubNonEmpty

    -- * Fold1s with Fold counterparts
    , length
    , and
    , or
    , all
    , any
    , sum
    , product
    , elem
    , notElem
    , find
    , index
    , elemIndex
    , findIndex
    , lookup
    , Control.Foldl1.mapM_

    -- * Generic Fold1s with Fold counterparts
    , genericLength
    , genericIndex

    -- * Container Fold1s with Fold counterparts
    , list

    -- * Utilities with Fold counterparts
    -- $utilities
    , purely
    , purely_
    , impurely
    , impurely_
    , generalize
    , simplify
    , hoists
    , premap
    , premapM
    , FromMaybe(..)
    , Handler1
    , handles
    , foldOver
    , FromMaybeM(..)
    , HandlerM1
    , handlesM
    , foldOverM
    , folded1
    , nest

    -- * Re-exports
    -- $reexports
    , module Data.Foldable1
    ) where

import Control.Applicative
import Data.Functor.Identity (Identity, runIdentity)
import Data.List.NonEmpty (NonEmpty (..) )
import Data.Monoid hiding ((<>))
import Data.Profunctor ( Choice(..), Profunctor(..) )
import Data.Foldable1 (Foldable1)
import Prelude hiding
    ( head
    , tail
    , last
    , null
    , length
    , and
    , or
    , all
    , any
    , sum
    , product
    , maximum
    , minimum
    , elem
    , notElem
    , lookup
    , map
    , either
    , drop
    )

import Control.Foldl (Fold (..), FoldM (..))
import Control.Foldl qualified as L
import Control.Foldl.NonEmpty qualified as LNE
import Data.Foldable1 qualified as F1
import Data.List.NonEmpty qualified as NE
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Semigroup.Foldable (traverse1_)
import Data.Functor.Apply (Apply (liftF2))
import Control.Monad ((<=<))

data Pair a b = Pair !a !b

{-| Efficient representation of a left fold that preserves the fold's step
    function, initial accumulator, and extraction function

    This allows the 'Applicative' instance to assemble derived folds that
    traverse the container only once

    A \''Fold1' a b\' processes elements of type __a__ and results in a
    value of type __b__.
-}
data Fold1 a b
  -- | @Fold1 @ @ initial @ @ step @ @ extract@
  = forall x. Fold1 !(a -> x) !(x -> a -> x) !(x -> b)

instance Functor (Fold1 a) where
    fmap f (Fold1 begin step done) = Fold1 begin step (f . done)
    {-# INLINE fmap #-}

instance Profunctor Fold1 where
    lmap = premap
    rmap = fmap

instance Choice Fold1 where
  right' (Fold1 begin step done) = Fold1 (fmap begin) (liftA2 step) (fmap done)
  {-# INLINE right' #-}

instance Applicative (Fold1 a) where
    pure b    = Fold1 (\_ -> ()) (\() _ -> ()) (\() -> b)
    {-# INLINE pure #-}

    (Fold1 beginL stepL doneL) <*> (Fold1 beginR stepR doneR) =
        let begin a = Pair (beginL a) (beginR a)
            step (Pair xL xR) a = Pair (stepL xL a) (stepR xR a)
            done (Pair xL xR) = doneL xL (doneR xR)
        in  Fold1 begin step done
    {-# INLINE (<*>) #-}

instance Semigroup b => Semigroup (Fold1 a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance Monoid b => Monoid (Fold1 a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

instance Num b => Num (Fold1 a b) where
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

    negate = fmap negate
    {-# INLINE negate #-}

    abs = fmap abs
    {-# INLINE abs #-}

    signum = fmap signum
    {-# INLINE signum #-}

    (+) = liftA2 (+)
    {-# INLINE (+) #-}

    (*) = liftA2 (*)
    {-# INLINE (*) #-}

    (-) = liftA2 (-)
    {-# INLINE (-) #-}

instance Fractional b => Fractional (Fold1 a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance Floating b => Floating (Fold1 a b) where
    pi = pure pi
    {-# INLINE pi #-}

    exp = fmap exp
    {-# INLINE exp #-}

    sqrt = fmap sqrt
    {-# INLINE sqrt #-}

    log = fmap log
    {-# INLINE log #-}

    sin = fmap sin
    {-# INLINE sin #-}

    tan = fmap tan
    {-# INLINE tan #-}

    cos = fmap cos
    {-# INLINE cos #-}

    asin = fmap asin
    {-# INLINE asin #-}

    atan = fmap atan
    {-# INLINE atan #-}

    acos = fmap acos
    {-# INLINE acos #-}

    sinh = fmap sinh
    {-# INLINE sinh #-}

    tanh = fmap tanh
    {-# INLINE tanh #-}

    cosh = fmap cosh
    {-# INLINE cosh #-}

    asinh = fmap asinh
    {-# INLINE asinh #-}

    atanh = fmap atanh
    {-# INLINE atanh #-}

    acosh = fmap acosh
    {-# INLINE acosh #-}

    (**) = liftA2 (**)
    {-# INLINE (**) #-}

    logBase = liftA2 logBase
    {-# INLINE logBase #-}

{-| Like 'Fold1', but monadic.

    A \''FoldM1' m a b\' processes elements of type __a__ and
    results in a monadic value of type __m b__.
-}
data FoldM1 m a b =
  -- | @FoldM1 @ @ initial @ @ step @ @ extract@
  forall x . FoldM1 (a -> m x) (x -> a -> m x) (x -> m b)

instance Functor m => Functor (FoldM1 m a) where
    fmap f (FoldM1 step start done) = FoldM1 step start done'
      where
        done' x = fmap f $! done x
    {-# INLINE fmap #-}

instance Applicative m => Applicative (FoldM1 m a) where
    pure b = FoldM1 (\_ -> pure ()) (\() _ -> pure ()) (\() -> pure b)
    {-# INLINE pure #-}

    (FoldM1 beginL stepL doneL) <*> (FoldM1 beginR stepR doneR) =
        let begin a = Pair <$> beginL a <*> beginR a
            step (Pair xL xR) a = Pair <$> stepL xL a <*> stepR xR a
            done (Pair xL xR) = doneL xL <*> doneR xR
        in  FoldM1 begin step done
    {-# INLINE (<*>) #-}

instance Functor m => Profunctor (FoldM1 m) where
    rmap = fmap
    lmap f (FoldM1 begin step done) = FoldM1 begin' step' done
      where
        begin' a = begin (f a)
        step' x a = step x (f a)

instance (Semigroup b, Monad m) => Semigroup (FoldM1 m a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance (Monoid b, Monad m) => Monoid (FoldM1 m a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

instance (Monad m, Num b) => Num (FoldM1 m a b) where
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

    negate = fmap negate
    {-# INLINE negate #-}

    abs = fmap abs
    {-# INLINE abs #-}

    signum = fmap signum
    {-# INLINE signum #-}

    (+) = liftA2 (+)
    {-# INLINE (+) #-}

    (*) = liftA2 (*)
    {-# INLINE (*) #-}

    (-) = liftA2 (-)
    {-# INLINE (-) #-}

instance (Monad m, Fractional b) => Fractional (FoldM1 m a b) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance (Monad m, Floating b) => Floating (FoldM1 m a b) where
    pi = pure pi
    {-# INLINE pi #-}

    exp = fmap exp
    {-# INLINE exp #-}

    sqrt = fmap sqrt
    {-# INLINE sqrt #-}

    log = fmap log
    {-# INLINE log #-}

    sin = fmap sin
    {-# INLINE sin #-}

    tan = fmap tan
    {-# INLINE tan #-}

    cos = fmap cos
    {-# INLINE cos #-}

    asin = fmap asin
    {-# INLINE asin #-}

    atan = fmap atan
    {-# INLINE atan #-}

    acos = fmap acos
    {-# INLINE acos #-}

    sinh = fmap sinh
    {-# INLINE sinh #-}

    tanh = fmap tanh
    {-# INLINE tanh #-}

    cosh = fmap cosh
    {-# INLINE cosh #-}

    asinh = fmap asinh
    {-# INLINE asinh #-}

    atanh = fmap atanh
    {-# INLINE atanh #-}

    acosh = fmap acosh
    {-# INLINE acosh #-}

    (**) = liftA2 (**)
    {-# INLINE (**) #-}

    logBase = liftA2 logBase
    {-# INLINE logBase #-}

-- | Apply a strict left 'Fold1' to a 'Foldable1' container
fold1 :: Foldable1 t => Fold1 a b -> t a -> b
-- fold1 (Fold1 begin step done) as = done (F1.foldlMap1' begin step as)
fold1 (Fold1 begin step done) as0 = L.fold (Fold step (begin x) done) xs
  where
    x :| xs = F1.toNonEmpty as0
{-# INLINE fold1 #-}

-- | Like 'fold1', but monadic
foldM1 :: (Foldable1 t, Monad m) => FoldM1 m a b -> t a -> m b
foldM1 (FoldM1 begin step done) as0 = L.foldM (FoldM step (begin x) done) xs
  where
    x :| xs = F1.toNonEmpty as0
{-# INLINE foldM1 #-}


-- {-| Convert a strict left 'Fold1' into a scan

--     >>> L.scan L.length [1..5]
--     [0,1,2,3,4,5]
-- -}
-- scan :: forall a b. Fold1 a b -> [a] -> [b]
-- scan (Fold1 step begin done) as = F1.foldlMap1' nil cons as
--   where
--     nil :: a -> [b]
--     nil x = done (begin x):[]
--     cons :: [b] -> a -> [b]
--     cons a x = undefined --done x:(k $! step x a)
-- -- scan (Fold1 step begin done) as = F1.foldlMap1' nil cons as
-- --   --foldr cons nil as begin
-- --   where
-- --     nil      x = done x:[]
-- --     cons a k x = done x:(k $! step x a)
-- {-# INLINE scan #-}

-- {-| Convert a `Fold1` into a prescan for any `Traversable` type

--     \"Prescan\" means that the last element of the scan is not included

--     >>> L.prescan L.length [1..5]
--     [0,1,2,3,4]
-- -}
-- prescan :: Traversable t => Fold1 a b -> t a -> t b
-- prescan (Fold1 step begin done) as = bs
--   where
--     step' x a = (x', b)
--       where
--         x' = step x a
--         b  = done x
--     (_, bs) = mapAccumL step' begin as
-- {-# INLINE prescan #-}

-- {-| Convert a `Fold1` into a postscan for any `Traversable` type

--     \"Postscan\" means that the first element of the scan is not included

--     >>> L.postscan L.length [1..5]
--     [1,2,3,4,5]
-- -}
-- postscan :: Traversable t => Fold1 a b -> t a -> t b
-- postscan (Fold1 step begin done) as = bs
--   where
--     step' x a = (x', b)
--       where
--         x' = step x a
--         b  = done x'
--     (_, bs) = mapAccumL step' begin as
-- {-# INLINE postscan #-}

-- | Fold1 all values within a container using 'mappend' and 'mempty'
sconcat :: Semigroup a => Fold1 a a
sconcat = Fold1 id (<>) id
{-# INLINABLE sconcat #-}

-- | Convert a \"@foldMap1@\" to a 'Fold1'
foldMap1 :: Semigroup w => (a -> w) -> (w -> b) -> Fold1 a b
foldMap1 to = Fold1 to (\x a -> (<>) x (to a))
{-# INLINABLE foldMap1 #-}

-- | Convert a \"@foldl1'@\" to a 'Fold1'
foldl1 :: (a -> a -> a) -> Fold1 a a
foldl1 step = Fold1 id step id
{-# INLINABLE foldl1 #-}

{-| Get the first element of a container or return 'Nothing' if the container is
    empty
-}
head :: Fold1 a a
head = Fold1 id const id
{-# INLINABLE head #-}

{-| Get the last element of a container or return 'Nothing' if the container is
    empty
-}
last :: Fold1 a a
last = Fold1 id (flip const) id
{-# INLINABLE last #-}

-- | Variant of 'Control.Foldl.length' that folds at least one element.
length :: Fold1 a Int
length = fromFold L.length
{-# INLINABLE length #-}

-- | Variant of 'Control.Foldl.and' that folds at least one element.
and :: Fold1 Bool Bool
and = fromFold L.and
{-# INLINABLE and #-}

-- | Variant of 'Control.Foldl.or' that folds at least one element.
or :: Fold1 Bool Bool
or = fromFold L.or
{-# INLINABLE or #-}

-- | Variant of 'Control.Foldl.all' that folds at least one element.
all :: (a -> Bool) -> Fold1 a Bool
all = fromFold . L.all
{-# INLINABLE all #-}

-- | Variant of 'Control.Foldl.any' that folds at least one element.
any :: (a -> Bool) -> Fold1 a Bool
any = fromFold . L.any
{-# INLINABLE any #-}

-- | Variant of 'Control.Foldl.sum' that folds at least one element.
sum :: Num a => Fold1 a a
sum = fromFold L.sum
{-# INLINABLE sum #-}

-- | Variant of 'Control.Foldl.product' that folds at least one element.
product :: Num a => Fold1 a a
product = fromFold L.product
{-# INLINABLE product #-}

-- | Computes the maximum element
maximum :: Ord a => Fold1 a a
maximum = Fold1 id max id
{-# INLINABLE maximum #-}

{-| Computes the maximum element with respect to the given comparison
    function
-}
maximumBy :: (a -> a -> Ordering) -> Fold1 a a
maximumBy cmp = Fold1 id max' id
  where
    max' x y = case cmp x y of
        GT -> x
        _  -> y
{-# INLINABLE maximumBy #-}

-- | Computes the minimum element
minimum :: Ord a => Fold1 a a
minimum = Fold1 id min id
{-# INLINABLE minimum #-}

{-| Computes the minimum element with respect to the given comparison
    function
-}
minimumBy :: (a -> a -> Ordering) -> Fold1 a a
minimumBy cmp = Fold1 id min' id
  where
    min' x y = case cmp x y of
        GT -> y
        _  -> x
{-# INLINABLE minimumBy #-}

-- | Variant of 'Control.Foldl.elem' that folds at least one element.
elem :: Eq a => a -> Fold1 a Bool
elem = fromFold . L.elem
{-# INLINABLE elem #-}

-- | Variant of 'Control.Foldl.notElem' that folds at least one element.
notElem :: Eq a => a -> Fold1 a Bool
notElem = fromFold . L.notElem
{-# INLINABLE notElem #-}

-- | Variant of 'Control.Foldl.find' that folds at least one element.
find :: (a -> Bool) -> Fold1 a (Maybe a)
find = fromFold . L.find
{-# INLINABLE find #-}

-- | Variant of 'Control.Foldl.index' that folds at least one element.
index :: Int -> Fold1 a (Maybe a)
index = fromFold . L.index
{-# INLINABLE index #-}

-- | Variant of 'Control.Foldl.elemIndex' that folds at least one element.
elemIndex :: Eq a => a -> Fold1 a (Maybe Int)
elemIndex = fromFold . L.elemIndex
{-# INLINABLE elemIndex #-}

-- | Variant of 'Control.Foldl.findIndex' that folds at least one element.
findIndex :: (a -> Bool) -> Fold1 a (Maybe Int)
findIndex = fromFold . L.findIndex
{-# INLINABLE findIndex #-}

-- | Variant of 'Control.Foldl.lookup' that folds at least one element.
lookup :: Eq k => k -> Fold1 (k, a) (Maybe a)
lookup = fromFold . L.lookup
{-# INLINABLE lookup #-}

-- | Converts an effectful function to a fold. Specialized version of 'sink'.
mapM_ :: Monad m => (a -> m ()) -> FoldM1 m a ()
mapM_ = sink
{-# INLINABLE mapM_ #-}

{-| Converts an effectful function to a fold

> sink (f <> g) = sink f <> sink g -- if `(<>)` is commutative
> sink mempty = mempty
-}


sink ::  (Semigroup w, Monad m) => (a -> m w) -> FoldM1 m a w
sink act = FoldM1 act step return  where
  step m a = do
    m' <- act a
    return $! (<>) m m'
{-# INLINABLE sink #-}

-- | Convert any `Fold` to a `Fold1`
fromFold :: Fold a b -> Fold1 a b
fromFold (Fold step begin done) = Fold1 (step begin) step done
{-# INLINABLE fromFold #-}

fromFoldM :: Monad m => FoldM m a b -> FoldM1 m a b
fromFoldM (FoldM step begin done) = FoldM1 (\a -> begin >>= flip step a) step done
{-# INLINABLE fromFoldM #-}

-- | Convert any `Fold1` to a `Fold`
toFold :: Fold1 a b -> Fold a (Maybe b)
toFold (Fold1 begin step done) = Fold step' begin' done'
  where
    begin'    = Nothing
    step' x a = Just $ maybe begin step x a
    done'     = fmap done


fromFoldNE :: LNE.Fold1 a b -> Fold1 a b
fromFoldNE (LNE.Fold1 (f :: a -> L.Fold a b)) = Fold1 begin' step' done'
  where
    done' :: Fold a b -> b
    done' (L.Fold _step begin done) = done begin

    step' :: Fold a b -> a -> Fold a b
    step' (L.Fold step begin done) a = L.Fold step (step begin a) done

    begin' :: a -> Fold a b
    begin' = f

toFoldNE :: Fold1 a b -> LNE.Fold1 a b
toFoldNE (Fold1 begin step done) =
  LNE.Fold1 $ \a -> Fold step (begin a) done

-- | Variant of 'Control.Foldl.genericLength' that folds at least one element.
genericLength :: Num b => Fold1 a b
genericLength = fromFold L.genericLength

-- | Variant of 'Control.Foldl.genericIndex' that folds at least one element.
genericIndex :: Integral i => i -> Fold1 a (Maybe a)
genericIndex = fromFold . L.genericIndex

-- | Fold all values into a non-empty list
nonEmpty :: Fold1 a (NonEmpty a)
nonEmpty = (:|) <$> head <*> tail
  where
    tail = fromFold $ L.drop 1 L.list
{-# INLINABLE nonEmpty #-}

-- | Fold all values into a non-empty list, in reverse order
revNonEmpty :: Fold1 a (NonEmpty a)
revNonEmpty = Fold1 NE.singleton (flip NE.cons) id
{-# INLINABLE revNonEmpty #-}

-- | Variant of 'Control.Foldl.list' that folds at least one element.
--
-- Variant of 'nonEmpty' that forgets the non-empty invariant of its result.
list :: Fold1 a [a]
list = fromFold L.list

-- {-| /O(n log n)/.  Fold values into a non-empty list with duplicates removed, while
--     preserving their first occurrences
-- -}
-- nubNonEmpty :: Ord a => Fold1 a (NonEmpty a)
-- nubNonEmpty = Fold1 step (Pair Set.empty id) fin
--   where
--     step (Pair s r) a = if Set.member a s
--       then Pair s r
--       else Pair (Set.insert a s) (r . (a :))
--     fin (Pair _ r) = r []
-- {-# INLINABLE nubNonEmpty #-}

-- {-| /O(n^2)/.  Fold1 values into a list with duplicates removed, while preserving
--     their first occurrences
-- -}
-- eqNubNonEmpty :: Eq a => Fold1 a [a]
-- eqNubNonEmpty = Fold1 step (Pair [] id) fin
--   where
--     step (Pair known r) a = if List.elem a known
--       then Pair known r
--       else Pair (a : known) (r . (a :))
--     fin (Pair _ r) = r []
-- {-# INLINABLE eqNubNonEmpty #-}

-- | Upgrade a fold to accept the 'Fold1' type
purely :: (forall x . (a -> x) -> (x -> a -> x) -> (x -> b) -> r) -> Fold1 a b -> r
purely f (Fold1 begin step done) = f begin step done
{-# INLINABLE purely #-}

-- | Upgrade a more traditional fold to accept the `Fold1` type
purely_ :: (forall x . (a -> x) -> (x -> a -> x) -> x) -> Fold1 a b -> b
purely_ f (Fold1 begin step done) = done (f begin step)
{-# INLINABLE purely_ #-}

-- | Upgrade a monadic fold to accept the 'FoldM1' type
impurely
    :: (forall x . (a -> m x) -> (x -> a -> m x) -> (x -> m b) -> r)
    -> FoldM1 m a b
    -> r
impurely f (FoldM1 begin step done) = f begin step done
{-# INLINABLE impurely #-}

-- | Upgrade a more traditional monadic fold to accept the `FoldM1` type
impurely_
    :: Monad m
    => (forall x . (a -> m x) -> (x -> a -> m x) -> m x) -> FoldM1 m a b -> m b
impurely_ f (FoldM1 begin step done) = do
    x <- f begin step
    done x
{-# INLINABLE impurely_ #-}

{-| Generalize a `Fold1` to a `FoldM1`

> generalize (pure r) = pure r
>
> generalize (f <*> x) = generalize f <*> generalize x
-}
generalize :: Monad m => Fold1 a b -> FoldM1 m a b
generalize (Fold1 begin step done) = FoldM1 begin' step' done'
  where
    begin' a  = return (begin a)
    step' x a = return (step x a)
    done' x   = return (done x)
{-# INLINABLE generalize #-}

{-| Simplify a pure `FoldM1` to a `Fold1`

> simplify (pure r) = pure r
>
> simplify (f <*> x) = simplify f <*> simplify x
-}
simplify :: FoldM1 Identity a b -> Fold1 a b
simplify (FoldM1 begin step done) = Fold1 begin' step' done'
  where
    begin' a  = runIdentity (begin a)
    step' x a = runIdentity (step x a)
    done' x   = runIdentity (done x)
{-# INLINABLE simplify #-}

{- | Shift a 'FoldM1' from one monad to another with a morphism such as 'lift' or 'liftIO';
     the effect is the same as 'Control.Monad.Morph.hoist'.
-}
hoists :: (forall x . m x -> n x) -> FoldM1 m a b -> FoldM1 n a b
hoists phi (FoldM1 begin step done) = FoldM1  (phi . begin) (\a b -> phi (step a b)) (phi . done)
{-# INLINABLE hoists #-}

{-| @(premap f folder)@ returns a new 'Fold1' where f is applied at each step

> fold (premap f folder) list = fold folder (List.map f list)

>>> fold (premap Sum L.mconcat) [1..10]
Sum {getSum = 55}

>>> fold L.mconcat (List.map Sum [1..10])
Sum {getSum = 55}

> premap id = id
>
> premap (f . g) = premap g . premap f

> premap k (pure r) = pure r
>
> premap k (f <*> x) = premap k f <*> premap k x
-}
premap :: (a -> b) -> Fold1 b r -> Fold1 a r
premap f (Fold1 begin step done) = Fold1 begin' step' done
  where
    begin' a = begin (f a)
    step' x a = step x (f a)
{-# INLINABLE premap #-}

{-| @(premapM f folder)@ returns a new 'FoldM1' where f is applied to each input
    element

> premapM return = id
>
> premapM (f <=< g) = premap g . premap f

> premapM k (pure r) = pure r
>
> premapM k (f <*> x) = premapM k f <*> premapM k x
-}
premapM :: Monad m => (a -> m b) -> FoldM1 m b r -> FoldM1 m a r
premapM f (FoldM1 begin step done) = FoldM1 begin' step' done
  where
    begin' a = f a >>= begin
    step' x a = f a >>= step x
{-# INLINABLE premapM #-}

{-|
> instance Monad m => Semigroup (FromMaybe m a) where
>     mappend (FromMaybe f) (FromMaybe g) = FromMaybeM (f . Just . g)
-}
newtype FromMaybe b = FromMaybe { appFromMaybe :: Maybe b -> b }

instance Semigroup (FromMaybe b) where
    FromMaybe f <> FromMaybe g = FromMaybe (f . (Just $!) . g)
    {-# INLINE (<>) #-}

{-| A handler for the upstream input of a `Fold1`

    This is compatible with van Laarhoven optics as defined in the lens package.
    Any lens, fold1 or traversal1 will type-check as a `Handler1`.
-}
type Handler1 a b =
    forall x. (b -> Const (Dual (FromMaybe x)) b) -> a -> Const (Dual (FromMaybe x)) a
{-| @(handles1 t folder)@ transforms the input of a `Fold1` using a lens or
    traversal1:

> handles1 _1        :: Fold1 a r -> Fold1 (a, b) r
> handles1 traverse1 :: Traversable1 t => Fold1 a r -> Fold1 (t a) r
> handles1 folded    :: Foldable1    t => Fold1 a r -> Fold1 (t a) r

> handles1 id = id
>
> handles1 (f . g) = handle1 f . handles1 g

> handles1 t (pure r) = pure r
>
> handles1 t (f <*> x) = handles1 t f <*> handles1 t x
-}
handles :: forall a b r. Handler1 a b -> Fold1 b r -> Fold1 a r
handles k (Fold1 begin step done) = Fold1 begin' step' done
  where
    begin' = stepMaybeA Nothing
    step' x = stepMaybeA (Just $! x)
    stepMaybeA = flip (appFromMaybe . getDual . getConst . k (Const . Dual . FromMaybe . flip stepMaybeB))
    stepMaybeB = \case
      Nothing -> begin
      Just x  -> step x
{-# INLINABLE handles #-}

{- | @(foldOver f folder xs)@ folds all values from a Lens, Traversal, Prism or Fold with the given folder

>>> foldOver (_Just . both) Foldl.sum (Just (2, 3))
5

>>> foldOver (_Just . both) Foldl.sum Nothing
0

> Foldl1.foldOver f folder xs == Foldl1.fold folder (xs^..f)

> Foldl1.foldOver (folded.f) folder == Foldl1.fold (handles1 f folder)

> Foldl1.foldOver folded == Foldl1.fold

-}
foldOver :: Handler1 s a -> Fold1 a b -> s -> b
foldOver l (Fold1 (begin :: a -> x) (step :: x -> a -> x) (done :: x -> b)) =
    done . stepMaybeS Nothing
  where
    stepMaybeS = flip (appFromMaybe . getDual . getConst . l (Const . Dual . FromMaybe . flip stepMaybeA))
    stepMaybeA = \case
      Nothing -> begin
      Just x  -> step x
{-# INLINABLE foldOver #-}

{-|
> instance Monad m => Semigroup (FromMaybeM m a) where
>     mappend (FromMaybeM f) (FromMaybeM g) = FromMaybeM (f . Just <=< g)
-}
newtype FromMaybeM m a = FromMaybeM { appFromMaybeM :: Maybe a -> m a }

instance Monad m => Semigroup (FromMaybeM m a) where
    FromMaybeM f <> FromMaybeM g = FromMaybeM (f . (Just $!) <=< g)
    {-# INLINE (<>) #-}

type HandlerM1 m a b =
    forall x . (b -> Const (Dual (FromMaybeM m x)) b) -> a -> Const (Dual (FromMaybeM m x)) a

handlesM :: HandlerM1 m a b -> FoldM1 m b r -> FoldM1 m a r
handlesM k (FoldM1 begin step done) = FoldM1 begin' step' done
  where
    begin' = stepMaybeA Nothing
    step' b = stepMaybeA (Just $! b)
    stepMaybeA = flip (appFromMaybeM . getDual . getConst . k (Const . Dual . FromMaybeM . flip stepMaybeB))
    stepMaybeB x b = case x of
      Nothing -> begin b
      Just x' -> step x' b
{-# INLINABLE handlesM #-}

foldOverM :: Monad m => HandlerM1 m s a -> FoldM1 m a b -> s -> m b
foldOverM l (FoldM1 (begin :: a -> m x) (step :: x -> a -> m x) (done :: x -> m b)) s = do
    r <- stepMaybeS Nothing s
    done r
  where
    stepMaybeS = flip (appFromMaybeM . getDual . getConst . l (Const . Dual . FromMaybeM . flip stepMaybeA))
    stepMaybeA = \case
      Nothing -> begin
      Just x  -> step x
{-# INLINABLE foldOverM #-}

{-|
> handles1 folded1 :: Foldable1 t => Fold1 a r -> Fold1 (t a) r
-}
folded1
    :: (Contravariant f, Apply f, Foldable1 t)
    => (a -> f a) -> (t a -> f (t a))
folded1 k ts = contramap (\_ -> ()) (traverse1_ k ts)
{-# INLINABLE folded1 #-}

{-| Nest a fold in an Apply.
-}
nest :: Apply f => Fold1 a b -> Fold1 (f a) (f b)
nest (Fold1 i s e) =
    Fold1 (fmap i)
          (liftF2 s)
          (fmap e)
{-# INLINABLE nest #-}

{- $reexports
    @Data.Foldable1@ re-exports the 'Foldable1' type class
-}
