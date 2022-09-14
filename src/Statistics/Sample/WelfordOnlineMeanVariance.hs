{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Statistics.Sample.WelfordOnlineMeanVariance
  ( WelfordExistingAggregate(..)
  , addValue
  , finalize
  , nextValue
  , newWelfordAggregate
  , Mean
  , Variance
  , SampleVariance
  ) where

import           Control.DeepSeq
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
  = WelfordExistingAggregateEmpty
  | WelfordExistingAggregate
      { welfordCount :: !Int
      , welfordMean  :: !a
      , welfordM2    :: !a
      }
  deriving (Eq, Show, Read, Generic, NFData, Serialize)


-- | Create a new empty Aggreate for the calculation.
newWelfordAggregate :: WelfordExistingAggregate a
newWelfordAggregate = WelfordExistingAggregateEmpty


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


-- | Calculate mean, variance and sample variance from aggregate.
finalize :: (WelfordOnline a) => WelfordExistingAggregate a -> (Mean a, Variance a, SampleVariance a)
finalize (WelfordExistingAggregate count mean m2)
  | count < 2 = (mean, m2, m2)
  | otherwise = (mean, m2 `divideInt` count, m2 `divideInt` (count - 1))
finalize WelfordExistingAggregateEmpty = error "finalize: Emtpy Welford Online Aggreate. Add data first!"

-- | Add a new sample to the aggregate and compute mean and variances.
nextValue :: (WelfordOnline a) => WelfordExistingAggregate a -> a -> (WelfordExistingAggregate a, (Mean a, Variance a, SampleVariance a))
nextValue agg val =
  let agg' = addValue agg val
   in (agg', finalize agg')


-- # For a new value newValue, compute the new count, new mean, the new M2.
-- # mean accumulates the mean of the entire dataset
-- # M2 aggregates the squared distance from the mean
-- # count aggregates the number of samples seen so far
-- def update(existingAggregate, newValue):
--     (count, mean, M2) = existingAggregate
--     count' += 1
--     delta = newValue - mean
--     mean' += delta / count'
--     delta2 = newValue - mean'
--     M2' += delta * delta2
--     return (count', mean', M2')

-- # Retrieve the mean, variance and sample variance from an aggregate
-- def finalize(existingAggregate):
--     (count, mean, M2) = existingAggregate
--     if count < 2:
--         return float("nan")
--     else:
--         (mean, variance, sampleVariance) = (mean, M2 / count, M2 / (count - 1))
--         return (mean, variance, sampleVariance)
