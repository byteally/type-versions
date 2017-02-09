{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Type.Versions
  (
  -- * Versioning styles
    MajorMinor (..)
  , Major (..)
  , SemVar (..)
  
  -- * Building custom versions  
  , VersionOrd
  , compareVersion
  ) where

import           Data.Proxy
import           GHC.TypeLits

-- | Singleton class for ordering
class SingOrd (ord :: Ordering) where
  singOrd :: proxy ord -> Ordering

instance SingOrd 'EQ where
  singOrd = const EQ

instance SingOrd 'LT where
  singOrd = const LT

instance SingOrd 'GT where
  singOrd = const GT

-- | Defines ordering of versions.
type family VersionOrd (v1 :: k) (v2 :: k) :: Ordering

-- | Comparison between two versions. Returns an 'Ord'.
--             
-- >>> compareVersion (MajorMinor :: MajorMinor '(0, 0)) (MajorMinor :: MajorMinor '(0, 1)) == LT
-- True            
compareVersion :: forall v1 v2. (SingOrd (VersionOrd v1 v2)) => v1 -> v2 -> Ordering
compareVersion _v1 _v2 = singOrd $ (Proxy :: Proxy (VersionOrd v1 v2))

-- | A Style of versioning which has a Major version and a Minor version.
data MajorMinor (ver :: (Nat, Nat)) = MajorMinor

-- | A Style of versioning which has only has a Major version.
data Major (maj :: Nat) = Major

-- | A SemVar Style of versioning.
data SemVar (ver :: (Nat, Nat, Nat)) = SemVar

type instance VersionOrd (m :: Nat) (n :: Nat) = CmpNat m n
type instance VersionOrd (Major (m :: Nat)) (Major (n :: Nat)) = (CmpNat m n)
type instance VersionOrd (MajorMinor '(maj1, min1)) (MajorMinor '(maj2, min2)) = CmpIfEQ (CmpNat maj1 maj2) min1 min2
type instance VersionOrd (SemVar '(maj1, min1, pat1)) (SemVar '(maj2, min2, pat2))
  = CmpIfEQ (CmpIfEQ (CmpNat maj1 maj2) min1 min2) pat1 pat2

type family CmpIfEQ (res :: Ordering) (v1 :: k) (v2 :: k) :: Ordering where
  CmpIfEQ 'EQ v1 v2 = VersionOrd v1 v2
  CmpIfEQ o v1 v2   = o

{-
instance OrdVersion (Major maj) where
instance OrdVersion (MajorMinor '(maj, min)) where
instance OrdVersion (SemVar '(maj, min, pat))
-}
