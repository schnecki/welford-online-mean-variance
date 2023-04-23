{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
module Statistics.Sample.WelfordOnlineMeanVariance
  ( WelfordExistingAggregate(..)
  , WelfordOnline (..)
  , welfordNoNormalsiation
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
  , normaliseToZeroMeanUnitVariance
  , denormaliseFromZeroMeanUnitVariance
  , Mean
  , Variance
  , SampleVariance
  ) where

import           Control.Applicative   ((<|>))
import           Control.DeepSeq
import           Data.Maybe            (fromMaybe)
import           Data.Serialize
import qualified Data.Vector           as VB
import qualified Data.Vector.Serialize ()
import qualified Data.Vector.Storable  as VS
import qualified Data.Vector.Unboxed   as VU
import           GHC.Generics

type Mean a = a
type Variance a = a
type SampleVariance a = a


-- | For the storage of required information.
data WelfordExistingAggregate a
  = WelfordExistingAggregateEmpty [Int]
    -- ^ Emtpy aggregate. Needed as `a` can be of any type, which, hence, allows us to postpone determining `a` to when we receive the first value. Holds a list of indices for which the normalisation is disabled.
  | WelfordExistingAggregate
      { welfordNoNormalisation_ :: !(VB.Vector Int) -- ^ Indices for which to disable normalisation
      , welfordCountUnsafe      :: !Int
      , welfordMeanUnsafe       :: !a
      , welfordM2Unsafe         :: !a
      }
  deriving (Eq, Show, Read, Generic, NFData)

instance (Serialize a) => Serialize (WelfordExistingAggregate a) where
  get = (WelfordExistingAggregateEmpty <$> get) <|>
        (WelfordExistingAggregate <$> get <*> get <*> get <*> get ) <|>
        (fromOld <$> get)
    where fromOld WelfordExistingAggregateEmptyOld = WelfordExistingAggregateEmpty []
          fromOld (WelfordExistingAggregateOld a b c) = WelfordExistingAggregate VB.empty a b c

-- | For the storage of required information.
data WelfordExistingAggregateOld a
  = WelfordExistingAggregateEmptyOld
    -- ^ Emtpy aggregate. Needed as `a` can be of any type, which, hence, allows us to postpone determining `a` to when we receive the first value. Holds a list of indices for which the normalisation is disabled.
  | WelfordExistingAggregateOld
      { welfordCountUnsafeOld :: !Int
      , welfordMeanUnsafeOld  :: !a
      , welfordM2UnsafeOld    :: !a
      }
  deriving (Eq, Show, Read, Generic, NFData, Serialize)


-- | Create a new empty Aggreate for the calculation.
newWelfordAggregate :: [Int] -> WelfordExistingAggregate a
newWelfordAggregate = WelfordExistingAggregateEmpty


-- | Create a new empty Aggreate by specifying an example `a` value. It is safe to use the `*Unsafe` record field selectors from `WelfordExistingAggregate a`, when creating the data structure using
-- this fuction.
newWelfordAggregateDef :: (WelfordOnline a) => [Int] -> a -> WelfordExistingAggregate a
newWelfordAggregateDef xs a = WelfordExistingAggregate (VB.fromList xs) 0 (a `minus` a) (a `minus` a)


-- | Check if it is aggregate is empty
isWelfordExistingAggregateEmpty :: WelfordExistingAggregate a -> Bool
isWelfordExistingAggregateEmpty WelfordExistingAggregateEmpty{} = True
isWelfordExistingAggregateEmpty _                               = False

-- | Indices for which the normalisation is disabled.
welfordNoNormalsiation :: WelfordExistingAggregate a -> [Int]
welfordNoNormalsiation (WelfordExistingAggregateEmpty xs)  = xs
welfordNoNormalsiation (WelfordExistingAggregate xs _ _ _) = VB.toList xs


-- | Get counter.
welfordCount :: WelfordExistingAggregate a -> Int
welfordCount WelfordExistingAggregateEmpty{}    = 0
welfordCount (WelfordExistingAggregate _ c _ _) = c


-- | Get mean safely, returns Nothing if `WelfordExistingAggregateEmpty` as the type of `a` (e.g. length of vector) is unknown.
mWelfordMean :: WelfordExistingAggregate a -> Maybe a
mWelfordMean WelfordExistingAggregateEmpty{}    = Nothing -- Cannot create value `a`
mWelfordMean (WelfordExistingAggregate _ _ m _) = Just m


-- | Get mean with specifying a default value.
welfordMeanDef :: (WelfordOnline a) => a -> WelfordExistingAggregate a -> a
welfordMeanDef a WelfordExistingAggregateEmpty{}    = a `minus` a
welfordMeanDef _ (WelfordExistingAggregate _ _ m _) = m


-- | Class for all data structures that can be used to computer the Welford approximation.
-- For instance, this can be used to compute the Welford algorithm on a `Vector`s of `Fractional`,
-- while only requiring to handle one `WelfordExistingAggregate`.
class WelfordOnline a where
  plus :: a -> a -> a                  -- ^ Addition.
  minus :: a -> a -> a                 -- ^ Subtraction.
  multiply :: a -> a -> a              -- ^ Multiplication.
  divide :: a -> a -> a                -- ^ Division.
  divideInt :: a -> Int -> a           -- ^ Division by Integer.
  constInt :: a -> Int -> a            -- ^ Takes one example vector and a integer value and returns a vector of this integer.
  replaceIndex :: a -> ([Int], a) -> a -- ^ Replace given indices from first index with the ones from the seconds (if applicable).
  squareRootMax :: a -> a              -- ^ Compute the square root. Ensure the output is >=1e-3. Used for normalisation.
  clipValue :: Double -> a -> a        -- ^ Clip the value(s) to the given range. Used for normalisation.

infixl 7  `multiply`, `divideInt`, `divide`
infixl 6  `plus`, `minus`


instance (WelfordOnline a) => WelfordOnline (VB.Vector a) where
  plus          = VB.zipWith plus
  {-# INLINE plus #-}
  minus         = VB.zipWith minus
  {-# INLINE minus #-}
  multiply      = VB.zipWith multiply
  {-# INLINE multiply #-}
  divide        = VB.zipWith divide
  {-# INLINE divide #-}
  divideInt x i = VB.map (`divideInt` i) x
  {-# INLINE divideInt #-}
  constInt x n  = VB.map (`constInt` n) x
  {-# INLINE constInt #-}
  replaceIndex x (is, y) = x VB.// map (\i -> (i, y VB.! i)) (filter (\i -> i < lenx && i < leny) is)
    where lenx = VB.length x
          leny = VB.length y
  {-# INLINE replaceIndex #-}
  squareRootMax = VB.map squareRootMax
  {-# INLINE squareRootMax #-}
  clipValue v   = VB.map (clipValue v)
  {-# INLINE clipValue #-}

instance (WelfordOnline a, VS.Storable a) => WelfordOnline (VS.Vector a) where
  plus          = VS.zipWith plus
  {-# INLINE plus #-}
  minus         = VS.zipWith minus
  {-# INLINE minus #-}
  multiply      = VS.zipWith multiply
  {-# INLINE multiply #-}
  divide        = VS.zipWith divide
  {-# INLINE divide #-}
  divideInt x i = VS.map (`divideInt` i) x
  {-# INLINE divideInt #-}
  constInt x n  = VS.map (`constInt` n) x
  {-# INLINE constInt #-}
  replaceIndex x (is, y) = x VS.// map (\i -> (i, y VS.! i)) (filter (\i -> i < lenx && i < leny) is)
    where lenx = VS.length x
          leny = VS.length y
  {-# INLINE replaceIndex #-}
  squareRootMax = VS.map squareRootMax
  {-# INLINE squareRootMax #-}
  clipValue v   = VS.map (clipValue v)
  {-# INLINE clipValue #-}


instance (WelfordOnline a, VU.Unbox a) => WelfordOnline (VU.Vector a) where
  plus          = VU.zipWith plus
  {-# INLINE plus #-}
  minus         = VU.zipWith minus
  {-# INLINE minus #-}
  multiply      = VU.zipWith multiply
  {-# INLINE multiply #-}
  divide        = VU.zipWith divide
  {-# INLINE divide #-}
  divideInt x i = VU.map (`divideInt` i) x
  {-# INLINE divideInt #-}
  constInt x n  = VU.map (`constInt` n) x
  {-# INLINE constInt #-}
  replaceIndex x (is, y) = x VU.// map (\i -> (i, y VU.! i)) (filter (\i -> i < lenx && i < leny) is)
    where lenx = VU.length x
          leny = VU.length y
  {-# INLINE replaceIndex #-}
  squareRootMax = VU.map squareRootMax
  {-# INLINE squareRootMax #-}
  clipValue v   = VU.map (clipValue v)
  {-# INLINE clipValue #-}


instance WelfordOnline Double where
  plus          = (+)
  {-# INLINE plus #-}
  minus         = (-)
  {-# INLINE minus #-}
  multiply      = (*)
  {-# INLINE multiply #-}
  divide        = (/)
  {-# INLINE divide #-}
  divideInt x i = x / fromIntegral i
  {-# INLINE divideInt #-}
  constInt _ n  = fromIntegral n
  {-# INLINE constInt #-}
  replaceIndex x _ = x
  {-# INLINE replaceIndex #-}
  squareRootMax = max 1e-3 . sqrt
  {-# INLINE squareRootMax #-}
  clipValue v   = min v . max (-v)
  {-# INLINE clipValue #-}


instance WelfordOnline Float where
  plus          = (+)
  {-# INLINE plus #-}
  minus         = (-)
  {-# INLINE minus #-}
  multiply      = (*)
  {-# INLINE multiply #-}
  divide        = (/)
  {-# INLINE divide #-}
  divideInt x i = x / fromIntegral i
  {-# INLINE divideInt #-}
  constInt _ n  = fromIntegral n
  {-# INLINE constInt #-}
  replaceIndex x _ = x
  {-# INLINE replaceIndex #-}
  squareRootMax = max 1e-3 . sqrt
  {-# INLINE squareRootMax #-}
  clipValue v   = min v' . max (-v')
    where v' = realToFrac v
  {-# INLINE clipValue #-}


instance WelfordOnline Rational where
  plus          = (+)
  {-# INLINE plus #-}
  minus         = (-)
  {-# INLINE minus #-}
  multiply      = (*)
  {-# INLINE multiply #-}
  divide        = (/)
  {-# INLINE divide #-}
  divideInt x i = x / fromIntegral i
  {-# INLINE divideInt #-}
  constInt _ n  = fromIntegral n
  {-# INLINE constInt #-}
  replaceIndex x _ = x
  {-# INLINE replaceIndex #-}
  squareRootMax = toRational . max (1e-3 :: Double) . sqrt . fromRational
  {-# INLINE squareRootMax #-}
  clipValue v   = min v' . max (-v')
    where v' = toRational v
  {-# INLINE clipValue #-}


-- | Add one value to the current aggregate.
addValue :: (WelfordOnline a) => WelfordExistingAggregate a -> a -> WelfordExistingAggregate a
addValue (WelfordExistingAggregate dis count mean m2) val =
  let count' = count + 1
      delta = val `minus` mean
      mean' = mean `plus` (delta `divideInt` count')
      delta2 = val `minus` mean'
      m2' = m2 `plus` (delta `multiply` delta2)
   in WelfordExistingAggregate dis count' mean' m2'
addValue (WelfordExistingAggregateEmpty dis) val =
  let count' = 1
      delta' = val
      mean' = delta' `divideInt` count'
      delta2' = val `minus` mean'
      m2' = delta' `multiply` delta2'
   in WelfordExistingAggregate (VB.fromList dis) count' mean' m2'

-- | Add multiple values to the current aggregate. This is `foldl addValue`.
addValues :: (WelfordOnline a, Foldable f) => WelfordExistingAggregate a -> f a -> WelfordExistingAggregate a
addValues = foldl addValue


-- | Calculate mean, variance and sample variance from aggregate. Calls `error` for `WelfordExistingAggregateEmpty`.
finalize :: (WelfordOnline a) => WelfordExistingAggregate a -> (Mean a, Variance a, SampleVariance a)
finalize = fromMaybe err . mFinalize
  where err = error "Statistics.Sample.WelfordOnlineMeanVariance.finalize: Emtpy Welford Online Aggreate. Add data first!"


-- | Calculate mean, variance and sample variance from aggregate. Safe function.
mFinalize :: (WelfordOnline a) => WelfordExistingAggregate a -> Maybe (Mean a, Variance a, SampleVariance a)
mFinalize (WelfordExistingAggregate _ count mean m2)
  | count < 2 = Just (mean, m2, m2)
  | otherwise = Just (mean, m2 `divideInt` count, m2 `divideInt` (count - 1))
mFinalize WelfordExistingAggregateEmpty{} = Nothing


-- | Add a new sample to the aggregate and compute mean and variances.
nextValue :: (WelfordOnline a) => WelfordExistingAggregate a -> a -> (WelfordExistingAggregate a, (Mean a, Variance a, SampleVariance a))
nextValue agg val =
  let agg' = addValue agg val
   in (agg', finalize agg')


-- | Normalise the given input assuming the learned standard deviation with sample variance to zero mean
-- and unit variance. For the first 100 values the output is clipped to @(-3, 3)@.
normaliseToZeroMeanUnitVariance :: (WelfordOnline a) => WelfordExistingAggregate a -> a -> a
normaliseToZeroMeanUnitVariance WelfordExistingAggregateEmpty{} x = clipValue 3 x
normaliseToZeroMeanUnitVariance wel x
  | welfordCountUnsafe wel < 100 = clipValue 3 $ (x `minus` mean) `divide` squareRootMax variance
  | otherwise = resetIndices $ (x `minus` mean) `divide` squareRootMax variance
  where
    resetIndices x'
      | null dis = x'
      | otherwise = replaceIndex x' (dis, x)
    dis = welfordNoNormalsiation wel
    (mean, _, variance) = finalize wel


-- | Denormalise from a zero mean unit variance normalised value (see `normaliseToZeroMeanUnitVariance`) to
-- the original value(s).
denormaliseFromZeroMeanUnitVariance :: (WelfordOnline a) => WelfordExistingAggregate a -> a -> a
denormaliseFromZeroMeanUnitVariance WelfordExistingAggregateEmpty{} x = x
denormaliseFromZeroMeanUnitVariance wel x = resetIndices $ (x `multiply` squareRootMax variance) `plus` mean
  where
    resetIndices x'
      | null dis = x'
      | otherwise = replaceIndex x' (dis, x)
    dis = welfordNoNormalsiation wel
    (mean, _, variance) = finalize wel
