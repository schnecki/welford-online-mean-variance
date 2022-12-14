{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Statistics.Sample.WelfordOnlineMeanVariance
  ( WelfordExistingAggregate(..)
  , WelfordOnline (..)
  , newWelfordAggregateDef
  , newWelfordAggregate
  , welfordCount
  , mWelfordMean
  , welfordMeanDef
  , addValue
  , addValues
  , mFinalize
  , finalize
  , nextValue
  , isWelfordExistingAggregateEmpty
  , Mean
  , Variance
  , SampleVariance
  ) where

import           Control.DeepSeq
import           Data.Maybe           (fromMaybe)
import           Data.Serialize
import qualified Data.Vector          as VB
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import           GHC.Generics

type Mean a = a
type Variance a = a
type SampleVariance a = a


-- | For the storage of required information.
data WelfordExistingAggregate a
  = WelfordExistingAggregateEmpty -- ^ Emtpy aggregate. Needed as `a` can be of any type, which, hence, allows us to postpone determining `a` to when we receive the first value.
  | WelfordExistingAggregate
      { welfordCountUnsafe :: !Int
      , welfordMeanUnsafe  :: !a
      , welfordM2Unsafe    :: !a
      }
  deriving (Eq, Show, Read, Generic, NFData, Serialize)

-- | Create a new empty Aggreate for the calculation.
newWelfordAggregate :: WelfordExistingAggregate a
newWelfordAggregate = WelfordExistingAggregateEmpty

-- | Create a new empty Aggreate by specifying an example `a` value. It is safe to use the `*Unsafe` record field selectors from `WelfordExistingAggregate a`, when creating the data structure using
-- this fuction.
newWelfordAggregateDef :: (WelfordOnline a) => a -> WelfordExistingAggregate a
newWelfordAggregateDef a = WelfordExistingAggregate 0 (a `minus` a) (a `minus` a)

-- | Check if it is aggregate is empty
isWelfordExistingAggregateEmpty :: WelfordExistingAggregate a -> Bool
isWelfordExistingAggregateEmpty WelfordExistingAggregateEmpty = True
isWelfordExistingAggregateEmpty _                             = False

-- | Get counter safely, returns Nothing if `WelfordExistingAggregateEmpty`.
welfordCount :: WelfordExistingAggregate a -> Int
welfordCount WelfordExistingAggregateEmpty    = 0
welfordCount (WelfordExistingAggregate c _ _) = c

-- | Get counter safely, returns Nothing if `WelfordExistingAggregateEmpty`.
mWelfordMean :: WelfordExistingAggregate a -> Maybe a
mWelfordMean WelfordExistingAggregateEmpty    = Nothing -- Cannot create value `a`
mWelfordMean (WelfordExistingAggregate _ m _) = Just m

-- | Get counter with specifying a default value.
welfordMeanDef :: (WelfordOnline a) => a -> WelfordExistingAggregate a -> a
welfordMeanDef a WelfordExistingAggregateEmpty    = a `minus` a
welfordMeanDef _ (WelfordExistingAggregate _ m _) = m

-- | Add one value to the current aggregate.
addValue :: (WelfordOnline a) => WelfordExistingAggregate a -> a -> WelfordExistingAggregate a
addValue (WelfordExistingAggregate count mean m2) val =
  let count' = count + 1
      delta = val `minus` mean
      mean' = mean `plus` (delta `divideInt` count')
      delta2 = val `minus` mean'
      m2' = m2 `plus` (delta `multiply` delta2)
   in WelfordExistingAggregate count' mean' m2'
addValue WelfordExistingAggregateEmpty val =
  let count' = 1
      delta' = val
      mean' = delta' `divideInt` count'
      delta2' = val `minus` mean'
      m2' = delta' `multiply` delta2'
   in WelfordExistingAggregate count' mean' m2'

-- | Add multiple values to the current aggregate. This is `foldl addValue`.
addValues :: (WelfordOnline a, Foldable f) => WelfordExistingAggregate a -> f a -> WelfordExistingAggregate a
addValues = foldl addValue

-- | Calculate mean, variance and sample variance from aggregate. Calls `error` for `WelfordExistingAggregateEmpty`.
finalize :: (WelfordOnline a) => WelfordExistingAggregate a -> (Mean a, Variance a, SampleVariance a)
finalize = fromMaybe err . mFinalize
  where err = error "Statistics.Sample.WelfordOnlineMeanVariance.finalize: Emtpy Welford Online Aggreate. Add data first!"

-- | Calculate mean, variance and sample variance from aggregate. Safe function.
mFinalize :: (WelfordOnline a) => WelfordExistingAggregate a -> Maybe (Mean a, Variance a, SampleVariance a)
mFinalize (WelfordExistingAggregate count mean m2)
  | count < 2 = Just (mean, m2, m2)
  | otherwise = Just (mean, m2 `divideInt` count, m2 `divideInt` (count - 1))
mFinalize WelfordExistingAggregateEmpty = Nothing


-- | Add a new sample to the aggregate and compute mean and variances.
nextValue :: (WelfordOnline a) => WelfordExistingAggregate a -> a -> (WelfordExistingAggregate a, (Mean a, Variance a, SampleVariance a))
nextValue agg val =
  let agg' = addValue agg val
   in (agg', finalize agg')


-- | Class for all data strucutres that can be used to computer the Welford approximation. For instance, this can be used to compute the Welford algorithm on a `Vector`s of `Fractional`, while only
-- requiring to handle one `WelfordExistingAggregate`.
class WelfordOnline a where
  plus :: a -> a -> a
  minus :: a -> a -> a
  multiply :: a -> a -> a
  divideInt :: a -> Int -> a


instance (WelfordOnline a) => WelfordOnline (VB.Vector a) where
  plus          = VB.zipWith plus
  {-# INLINE plus #-}
  minus         = VB.zipWith minus
  {-# INLINE minus #-}
  multiply      = VB.zipWith multiply
  {-# INLINE multiply #-}
  divideInt x i = VB.map (`divideInt` i) x
  {-# INLINE divideInt #-}

instance (WelfordOnline a, VS.Storable a) => WelfordOnline (VS.Vector a) where
  plus          = VS.zipWith plus
  {-# INLINE plus #-}
  minus         = VS.zipWith minus
  {-# INLINE minus #-}
  multiply      = VS.zipWith multiply
  {-# INLINE multiply #-}
  divideInt x i = VS.map (`divideInt` i) x
  {-# INLINE divideInt #-}


instance (WelfordOnline a, VU.Unbox a) => WelfordOnline (VU.Vector a) where
  plus          = VU.zipWith plus
  {-# INLINE plus #-}
  minus         = VU.zipWith minus
  {-# INLINE minus #-}
  multiply      = VU.zipWith multiply
  {-# INLINE multiply #-}
  divideInt x i = VU.map (`divideInt` i) x
  {-# INLINE divideInt #-}


instance WelfordOnline Double where
  plus = (+)
  {-# INLINE plus #-}
  minus = (-)
  {-# INLINE minus #-}
  multiply = (*)
  {-# INLINE multiply #-}
  divideInt x i = x / fromIntegral i
  {-# INLINE divideInt #-}

instance WelfordOnline Float where
  plus = (+)
  {-# INLINE plus #-}
  minus = (-)
  {-# INLINE minus #-}
  multiply = (*)
  {-# INLINE multiply #-}
  divideInt x i = x / fromIntegral i
  {-# INLINE divideInt #-}

instance WelfordOnline Rational where
  plus = (+)
  {-# INLINE plus #-}
  minus = (-)
  {-# INLINE minus #-}
  multiply = (*)
  {-# INLINE multiply #-}
  divideInt x i = x / fromIntegral i
  {-# INLINE divideInt #-}
