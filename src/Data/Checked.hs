-- | Type-indexed runtime-checked properties.
module Data.Checked
  ( Checked
  , Checked'
  , trustMe
  , untrust
  , trustThat
  , preserving
  , preserving'
  , checked
  , Property
  , property
  , maybeHolds
  , holds
  , check
  , checkAnd
  , I
  , U
  , Not
  , pand
  , (~&&)
  , por
  , (~||)
  , pnot
  , commute
  , associateL
  , associateR
  , construct
  , deconstruct
  ) where

import Control.DeepSeq (NFData(..))

-- | Wrapper-evidence for property /p/.
newtype Checked p v = Checked v

-- | Trusted evidence for property /p/.
newtype Checked' p v = Checked' v

instance NFData v => NFData (Checked p v) where
  rnf (Checked v) = rnf v

instance NFData v => NFData (Checked' p v) where
  rnf (Checked' v) = rnf v

class Checkable c where
    checkable :: v -> c p v
    -- | Unwrap the checked value.
    checked :: c p v -> v

instance Checkable Checked where
    checkable = Checked
    checked (Checked v) = v
    {-# INLINE checked #-}

instance Checkable Checked' where
    checkable = Checked'
    checked (Checked' v) = v
    {-# INLINE checked #-}

-- | Use when the property can be deduced without a runtime check.
trustMe :: v -> Checked p v
trustMe = Checked

-- | Demote a trusted checked value to an untrusted checked value
untrust :: Checked' p v -> Checked p v
untrust (Checked' v) = Checked v
{-# INLINE untrust #-}

-- | Use when the property can be deduced without a runtime check.
trustThat :: Checkable c => p -> v -> c p v
trustThat _ = checkable
{-# INLINE trustThat #-}

-- | Apply a fuction that preserves the property to the checked value.
preserving :: (v -> v) -> Checked p v -> Checked p v
preserving f c = checkable (f (checked c))
{-# INLINE preserving #-}

-- | Apply a fuction that preserves the property to the (possibly
-- trusted) checked value.
preserving' :: Checkable c => p -> (v -> v) -> c p v -> c p v
preserving' _ f c = checkable (f (checked c))
{-# INLINE preserving' #-}

newtype Property p v = Property {
  -- | Test if the property holds for the given value.
  --   The first argument is supposed to be ignored.
    holds :: v -> Bool }

property :: p -> (v -> Bool) -> Property p v
property _ = Property
{-# INLINABLE property #-}

-- | Return 'Just' /v/ if /p/ holds and 'Nothing' overwise.
maybeHolds :: Property p v -> v -> Maybe v
maybeHolds p v | holds p v = Just v
               | otherwise = Nothing
{-# INLINABLE maybeHolds #-}

-- | Wrap the value if the property holds.
check :: Checkable c => Property p v -> v -> Maybe (c p v)
check p v | holds p v = Just (checkable v)
          | otherwise = Nothing
{-# INLINABLE check #-}

-- | Add "and property" to the checked value if the property holds
checkAnd :: Checkable c => Property p2 v
    -> c p1 v
    -> Maybe (c (I p1 p2) v)
checkAnd p v | holds p (checked v) = Just (checkable . checked $ v)
             | otherwise = Nothing

-- | Property intersection
newtype I p1 p2 = I (p1, p2)

-- | Property union
newtype U p1 p2 = U (p1, p2)

-- | Property negation
newtype Not p1 = Not p1

-- | Property intersect (p1 AND p2). p1 is checked first
pand :: Property p1 v -> Property p2 v -> Property (I p1 p2) v
pand p1 p2 = Property (\v -> holds p1 v && holds p2 v)
{-# INLINABLE pand #-}

-- | Synonym for pand
(~&&) :: Property p1 v -> Property p2 v -> Property (I p1 p2) v
(~&&) = pand
{-# INLINABLE (~&&) #-}

-- | Property union (p1 OR p2)
por :: Property p1 v -> Property p2 v -> Property (U p1 p2) v
por p1 p2 = Property (\v -> holds p1 v || holds p2 v)
{-# INLINABLE por #-}

-- | Synonym for por
(~||) :: Property p1 v -> Property p2 v -> Property (U p1 p2) v
(~||) = por
{-# INLINABLE (~||) #-}

-- | Property negation (NOT c)
pnot :: Property p1 v -> Property (Not p1) v
pnot p = Property (not . holds p)

class Logical l where
    logic :: l p1 p2 -> l p1 p2

instance Logical I where
    logic = id

instance Logical U where
    logic = id

commute :: (Checkable c, Logical l) => c (l p1 p2) v -> c (l p2 p1) v
commute = checkable . checked
{-# INLINE commute #-}

associateL :: (Checkable c, Logical l) => c (l p1 (l p2 p3)) v
    -> Checked (l (l p1 p2) p3) v
associateL = checkable . checked
{-# INLINE associateL #-}

associateR :: (Checkable c, Logical l) => c (l (l p1 p2) p3) v
    -> c (l p1 (l p2 p3)) v
associateR = checkable . checked
{-# INLINE associateR #-}

construct :: Checkable c => c p1 v -> c (U p1 p2) v
construct = checkable . checked
{-# INLINE construct #-}

deconstruct :: Checkable c => c (I p1 p2) v -> c p1 v
deconstruct = checkable . checked
{-# INLINE deconstruct #-}
