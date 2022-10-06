{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Statistics.Sample.WelfordOnlineMeanVariance
  ( WelfordExistingAggregate(..)
  , WelfordOnline (..)
  , addValue
  , addValues
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

-- | Add multiple values to the current aggregate. This is `foldl addValue`.
addValues :: (WelfordOnline a, Foldable f) => WelfordExistingAggregate a -> f a -> WelfordExistingAggregate a
addValues = foldl addValue

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
