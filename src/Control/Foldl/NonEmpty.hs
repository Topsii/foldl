{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{-| This module provides a `Fold1` type that is a \"non-empty\" analog of the
    `Fold` type, meaning that it requires at least one input element in order to
    produce a result

    This module does not provide all of the same utilities as the
    "Control.Foldl" module.  Instead, this module only provides the utilities
    which can make use of the non-empty input guarantee (e.g. `head`).  For
    all other utilities you can convert them from the equivalent `Fold` using
    `fromFold`.

    Import this module qualified to avoid clashing with the Prelude:

>>> import qualified Control.Foldl.NonEmpty as Foldl1

    Use 'fold1' to apply a 'Fold1' to a list:

>>> Foldl1.fold1 Foldl1.last (1 :| [2..10])
10

-}

module Control.Foldl.NonEmpty (
    -- * Fold Types
      Fold1(..)

    -- * Folding
    , Control.Foldl.NonEmpty.fold1

    -- * Conversion between Fold and Fold1
    , fromFold
    , toFold

    -- * Folds
    , sconcat
    , head
    , last
    , maximum
    , maximumBy
    , minimum
    , minimumBy

    -- ** Non-empty Container Fold1s
    , nonEmpty

    -- -- * Fold1s with Fold counterparts
    -- , Control.Foldl.NonEmpty.length
    -- , Control.Foldl.NonEmpty.and
    -- , Control.Foldl.NonEmpty.or
    -- , Control.Foldl.NonEmpty.all
    -- , Control.Foldl.NonEmpty.any
    -- , Control.Foldl.NonEmpty.sum
    -- , Control.Foldl.NonEmpty.product
    -- , Control.Foldl.NonEmpty.elem
    -- , Control.Foldl.NonEmpty.notElem
    -- , find
    -- , index
    -- , elemIndex
    -- , findIndex


    -- -- ** Container Fold1s with Fold counterparts
    -- , list

    -- * Utilities
    , purely
    , purely_
    , premap
    , FromMaybe(..)
    , Handler1
    , handles
    , foldOver
    , folded1
    , Control.Foldl.NonEmpty.either
    ) where

import Control.Applicative (liftA2, Const(..))
import Control.Foldl (Fold(..))
import Control.Foldl.Internal (Either'(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Dual(..))
import Data.Functor.Apply (Apply)
import Data.Profunctor (Profunctor(..))
import Data.Semigroup.Foldable (Foldable1(..), traverse1_)
import Data.Functor.Contravariant (Contravariant(..))

import Prelude hiding (head, last, minimum, maximum)

import qualified Control.Foldl as Foldl


{- $setup

>>> import qualified Control.Foldl.NonEmpty as Foldl1
>>> import qualified Data.List as List
>>> import qualified Data.List.NonEmpty as NonEmpty
>>> import Data.Functor.Apply (Apply(..))
>>> import Data.Semigroup.Traversable (Traversable1(..))
>>> import Data.Monoid (Sum(..))

>>> _2 f (x, y) = fmap (\i -> (x, i)) (f y)

>>> both f (x, y) = (,) <$> f x <.> f y

-}

{-| A `Fold1` is like a `Fold` except that it consumes at least one input
    element
-}
data Fold1 a b = Fold1 (a -> Fold a b)

pattern Fold1_ :: forall a b. forall x. (a -> x) -> (x -> a -> x) -> (x -> b) -> Fold1 a b
pattern Fold1_ begin step done <- (toFold_ -> (begin, step, done))
  where Fold1_ begin step done = fromFold_ (begin, step, done)
{-# INLINABLE Fold1_ #-}
{-# COMPLETE Fold1_ :: Fold1 #-}

toFold_ :: Fold1 a b -> (a -> Fold a b, Fold a b -> a -> Fold a b, Fold a b -> b)
toFold_ (Fold1 (f :: a -> Fold a b)) = (begin', step', done')
  where
    done' :: Fold a b -> b
    done' (Fold _step begin done) = done begin

    step' :: Fold a b -> a -> Fold a b
    step' (Fold step begin done) a = Fold step (step begin a) done

    begin' :: a -> Fold a b
    begin' = f

fromFold_ :: forall a b. forall x. (a -> x, x -> a -> x, x -> b) -> Fold1 a b
fromFold_ (begin, step, done) =
  Fold1 $ \a -> Fold step (begin a) done

instance Functor (Fold1 a) where
    fmap f (Fold1 k) = Fold1 (fmap (fmap f) k)
    {-# INLINE fmap #-}

instance Profunctor Fold1 where
    lmap = premap
    {-# INLINE lmap #-}

    rmap = fmap
    {-# INLINE rmap #-}

instance Applicative (Fold1 a) where
    pure b = Fold1 (pure (pure b))
    {-# INLINE pure #-}

    Fold1 l <*> Fold1 r = Fold1 (liftA2 (<*>) l r)
    {-# INLINE (<*>) #-}

instance Semigroup b => Semigroup (Fold1 a b) where
    (<>) = liftA2 (<>)
    {-# INLINE (<>) #-}

instance Monoid b => Monoid (Fold1 a b) where
    mempty = pure mempty
    {-# INLINE mempty #-}

    mappend = (<>)
    {-# INLINE mappend #-}

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

-- | Apply a strict left `Fold1` to a `NonEmpty` list
fold1 :: Foldable1 f => Fold1 a b -> f a -> b
fold1 (Fold1 k) as1 = Foldl.fold (k a) as
  where
    a :| as = toNonEmpty as1
{-# INLINABLE fold1 #-}

-- | Promote any `Fold` to an equivalent `Fold1`
fromFold :: Fold a b -> Fold1 a b
fromFold (Fold step begin done) = Fold1 (\a -> Fold step (step begin a) done)
{-# INLINABLE fromFold #-}

-- | Promote any `Fold1` to an equivalent `Fold`
toFold :: Fold1 a b -> Fold a (Maybe b)
toFold (Fold1 k0) = Fold step begin done
  where
    begin = Left' k0

    step (Left' k) a = Right' (k a)
    step (Right' (Fold step' begin' done')) a =
        Right' (Fold step' (step' begin' a) done')

    done (Right' (Fold _ begin' done')) = Just (done' begin')
    done (Left' _) = Nothing
{-# INLINABLE toFold #-}

-- | Fold all values within a non-empty container into a `NonEmpty` list
nonEmpty :: Fold1 a (NonEmpty a)
nonEmpty = Fold1 (\a -> fmap (a :|) Foldl.list)
{-# INLINEABLE nonEmpty #-}

-- | Fold all values within a non-empty container using (`<>`)
sconcat :: Semigroup a => Fold1 a a
sconcat = Fold1 (\begin -> Fold (<>) begin id)
{-# INLINABLE sconcat #-}

-- | Get the first element of a non-empty container
head :: Fold1 a a
head = Fold1 (\begin -> Fold step begin id)
  where
    step a _ = a
{-# INLINABLE head #-}

-- | Get the last element of a non-empty container
last :: Fold1 a a
last = Fold1 (\begin -> Fold step begin id)
  where
    step _ a = a
{-# INLINABLE last #-}

-- -- | Variant of 'Control.Foldl.length' that folds at least one element.
-- length :: Fold1 a Int
-- length = fromFold Foldl.length
-- {-# INLINABLE length #-}

-- -- | Variant of 'Control.Foldl.and' that folds at least one element.
-- and :: Fold1 Bool Bool
-- and = fromFold Foldl.and
-- {-# INLINABLE and #-}

-- -- | Variant of 'Control.Foldl.or' that folds at least one element.
-- or :: Fold1 Bool Bool
-- or = fromFold Foldl.or
-- {-# INLINABLE or #-}

-- -- | Variant of 'Control.Foldl.all' that folds at least one element.
-- all :: (a -> Bool) -> Fold1 a Bool
-- all = fromFold . Foldl.all
-- {-# INLINABLE all #-}

-- -- | Variant of 'Control.Foldl.any' that folds at least one element.
-- any :: (a -> Bool) -> Fold1 a Bool
-- any = fromFold . Foldl.any
-- {-# INLINABLE any #-}

-- -- | Variant of 'Control.Foldl.sum' that folds at least one element.
-- sum :: Num a => Fold1 a a
-- sum = fromFold Foldl.sum
-- {-# INLINABLE sum #-}

-- -- | Variant of 'Control.Foldl.product' that folds at least one element.
-- product :: Num a => Fold1 a a
-- product = fromFold Foldl.product
-- {-# INLINABLE product #-}


-- | Computes the maximum element
maximum :: Ord a => Fold1 a a
maximum = Fold1 (\begin -> Fold max begin id)
{-# INLINABLE maximum #-}

-- | Computes the maximum element with respect to the given comparison function
maximumBy :: (a -> a -> Ordering) -> Fold1 a a
maximumBy cmp = Fold1 (\begin -> Fold max' begin id)
  where
    max' x y = case cmp x y of
        GT -> x
        _  -> y
{-# INLINABLE maximumBy #-}

-- | Computes the minimum element
minimum :: Ord a => Fold1 a a
minimum = Fold1 (\begin -> Fold min begin id)
{-# INLINABLE minimum #-}

-- | Computes the minimum element with respect to the given comparison function
minimumBy :: (a -> a -> Ordering) -> Fold1 a a
minimumBy cmp = Fold1 (\begin -> Fold min' begin id)
  where
    min' x y = case cmp x y of
        GT -> y
        _  -> x
{-# INLINABLE minimumBy #-}

-- -- | Variant of 'Control.Foldl.elem' that folds at least one element.
-- elem :: Eq a => a -> Fold1 a Bool
-- elem = fromFold . Foldl.elem
-- {-# INLINABLE elem #-}

-- -- | Variant of 'Control.Foldl.notElem' that folds at least one element.
-- notElem :: Eq a => a -> Fold1 a Bool
-- notElem = fromFold . Foldl.notElem
-- {-# INLINABLE notElem #-}

-- -- | Variant of 'Control.Foldl.find' that folds at least one element.
-- find :: (a -> Bool) -> Fold1 a (Maybe a)
-- find = fromFold . Foldl.find
-- {-# INLINABLE find #-}

-- -- | Variant of 'Control.Foldl.index' that folds at least one element.
-- index :: Int -> Fold1 a (Maybe a)
-- index = fromFold . Foldl.index
-- {-# INLINABLE index #-}

-- -- | Variant of 'Control.Foldl.elemIndex' that folds at least one element.
-- elemIndex :: Eq a => a -> Fold1 a (Maybe Int)
-- elemIndex = fromFold . Foldl.elemIndex
-- {-# INLINABLE elemIndex #-}

-- -- | Variant of 'Control.Foldl.findIndex' that folds at least one element.
-- findIndex :: (a -> Bool) -> Fold1 a (Maybe Int)
-- findIndex = fromFold . Foldl.findIndex
-- {-# INLINABLE findIndex #-}

-- -- | Variant of 'Control.Foldl.lookup' that folds at least one element.
-- lookup :: Eq k => k -> Fold1 (k, a) (Maybe a)
-- lookup = fromFold . Foldl.lookup
-- {-# INLINABLE lookup #-}

-- -- | Variant of 'Control.Foldl.list' that folds at least one element.
-- --
-- -- Variant of 'nonEmpty' that forgets the non-empty invariant of its result.
-- list :: Fold1 a [a]
-- list = fromFold Foldl.list

-- | Upgrade a fold to accept the 'Fold1' type
purely :: (forall x . (a -> x) -> (x -> a -> x) -> (x -> b) -> r) -> Fold1 a b -> r
purely f (Fold1_ begin step done) = f begin step done
{-# INLINABLE purely #-}

-- | Upgrade a more traditional fold to accept the `Fold1` type
purely_ :: (forall x . (a -> x) -> (x -> a -> x) -> x) -> Fold1 a b -> b
purely_ f (Fold1_ begin step done) = done (f begin step)
{-# INLINABLE purely_ #-}

{-| @(premap f folder)@ returns a new 'Fold1' where f is applied at each step

> Foldl1.fold1 (premap f folder) list = Foldl1.fold1 folder (List.map f list)

>>> Foldl1.fold1 (premap Sum Foldl1.sconcat) (1 :| [2..10])
Sum {getSum = 55}

>>> Foldl1.fold1 Foldl1.sconcat $ NonEmpty.map Sum (1 :| [2..10])
Sum {getSum = 55}

> premap id = id
>
> premap (f . g) = premap g . premap f

> premap k (pure r) = pure r
>
> premap k (f <*> x) = premap k f <*> premap k x
-}
premap :: (a -> b) -> Fold1 b r -> Fold1 a r
premap f (Fold1 k) = Fold1 k'
  where
    k' a = lmap f (k (f a))
{-# INLINABLE premap #-}


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

{-| @(handles t folder)@ transforms the input of a `Fold1` using a Lens,
    Traversal1, or Fold1 optic:

> handles _1        :: Fold1 a r -> Fold1 (a, b) r
> handles traverse1 :: Traversable1 t => Fold1 a r -> Fold1 (t a) r
> handles folded1   :: Foldable1    t => Fold1 a r -> Fold1 (t a) r

>>> Foldl1.fold1 (handles traverse1 Foldl1.nonEmpty) $ (1 :| [2..4]) :| [ 5 :| [6,7], 8 :| [9,10] ]
1 :| [2,3,4,5,6,7,8,9,10]

>>> Foldl1.fold1 (handles _2 Foldl1.sconcat) $ (1,"Hello ") :| [(2,"World"),(3,"!")]
"Hello World!"

> handles id = id
>
> handles (f . g) = handles f . handles g

> handles t (pure r) = pure r
>
> handles t (f <*> x) = handles t f <*> handles t x
-}
handles :: forall a b r. Handler1 a b -> Fold1 b r -> Fold1 a r
handles k (Fold1_ begin step done) = Fold1_ begin' step' done
  where
    begin' = stepAfromMaybe Nothing
    step' x = stepAfromMaybe (Just $! x)
    stepAfromMaybe = flip (appFromMaybe . getDual . getConst . k (Const . Dual . FromMaybe . flip stepBfromMaybe))
    stepBfromMaybe = maybe begin step
{-# INLINABLE handles #-}

{- | @(foldOver f folder xs)@ folds all values from a Lens, Traversal1 or Fold1 optic with the given folder

>>> foldOver (_2 . both) Foldl1.nonEmpty (1, (2, 3))
2 :| [3]

> Foldl1.foldOver f folder xs == Foldl1.fold1 folder (xs ^.. f)

> Foldl1.foldOver (folded1 . f) folder == Foldl1.fold1 (Foldl1.handles f folder)

> Foldl1.foldOver folded1 == Foldl1.fold1

-}
foldOver :: Handler1 s a -> Fold1 a b -> s -> b
foldOver l (Fold1_ begin step done) =
    done . stepSfromMaybe Nothing
  where
    stepSfromMaybe = flip (appFromMaybe . getDual . getConst . l (Const . Dual . FromMaybe . flip stepAfromMaybe))
    stepAfromMaybe = maybe begin step
{-# INLINABLE foldOver #-}

{-|
> handles folded1 :: Foldable1 t => Fold1 a r -> Fold1 (t a) r
-}
folded1
    :: (Contravariant f, Apply f, Foldable1 t)
    => (a -> f a) -> (t a -> f (t a))
folded1 k ts = contramap (\_ -> ()) (traverse1_ k ts)
{-# INLINABLE folded1 #-}

{-| Combine two folds into a fold over inputs for either of them.
-}
either :: Fold a1 b1 -> Fold a2 b2 -> Fold1 (Either a1 a2) (b1, b2)
either l r = fromFold $ Foldl.either l r
{-# INLINABLE either #-}
