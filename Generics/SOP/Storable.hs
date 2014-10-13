{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | A generic implementation of 'Storable'. By using meta-data, users have the
-- ability to specify custom layouts. There is also a default "tightly-packed"
-- default.

module Generics.SOP.Storable
  ( -- * Layouts
    Layout(..)
  , LayoutInfo(..)
  , HasLayout(..)
  , unarySequentialLayout

    -- * Generic 'Storable' Functions
  , gpeek
  , gpoke
  , gsizeOf
  ) where

import Control.Monad (void)
import Data.Proxy (Proxy(Proxy))
import Foreign (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import Generics.SOP (All, All2, Code, Generic, I(I), K(K), NP(Nil, (:*)), NS(Z),
                     POP(POP), SingI, SOP(SOP), from, hcliftA, hcliftA2,
                     hcollapse, hliftA, hpure, hsequence, hsequenceK, to, unK)

--------------------------------------------------------------------------------
storable :: Proxy Storable
storable = Proxy


--------------------------------------------------------------------------------
-- | Information about how a specific field should be laid out in memory.
data LayoutInfo = LayoutInfo
  { layoutFieldOffset :: Int
    -- ^ A byte offset from the initial data pointer to this field.
  }


--------------------------------------------------------------------------------
{-|

A specification for the memory layout of a data type. Usually, it is sufficient
to use a generic layout scheme, such as 'unarySequentialLayout'. However, there
are times when you may need to construct this by hand.

For example, the OpenGL shading language GLSL packs all vectors as @vec4@, so
the sequential layout will not work for pairs of 3-dimensional vectors:

> data T = T Vec3 Vec3

In this case, we construct our own 'Layout':

> tLayout :: Layout (Code T)
> tLayout = Layout
>   { layoutSize = sizeOf (undefined :: Vec4) * 2
>   , layoutOffsets = POP ((K 0 :* K (sizeOf (undefined :: Vec4)) :* Nil) :* Nil)
>   }

-}
data Layout code = Layout
  { layoutOffsets :: POP (K LayoutInfo) code
    -- ^ A specification of how each field is laid out.
  , layoutSize :: !Int
    -- ^ The overall size in memory that serialization of this data type requires.
  }


--------------------------------------------------------------------------------
-- | The class of data types that know their 'Storable' memory layout.
class HasLayout a where
  -- | Fetch the memory layout associated with @a@.
  layout :: proxy a -> Layout (Code a)


--------------------------------------------------------------------------------
-- | A memory layout suitable for data types that only have a single
-- constructor. Under this scheme, fields will be tightly packed in memory.
-- This layout corresponds to the typical use of @struct@ in C.
unarySequentialLayout :: forall a proxy xs. (Code a ~ '[xs], All Storable xs, SingI xs) => proxy a -> Layout (Code a)
unarySequentialLayout _ =
  Layout
    { layoutSize = sum (hcollapse sizes)
    , layoutOffsets = POP (hliftA (K . LayoutInfo . unK) offsets :* Nil)
    }

  where
  sizes :: NP (K Int) xs
  sizes =
    hcliftA storable
            (\(_ :: K () x) ->  K (sizeOf (undefined :: x)))
            (hpure (K ()) :: NP (K ()) xs)

  offsets =
    -- This is essentialy 'scanl (+) 0' over the sizes of fields in the record
    let go :: Num i => i -> i -> NP (K i) c -> NP (K i) c
        go _ _ Nil = Nil
        go prec precSize (K y :* ys) =
          K (prec + precSize) :*
          go (prec + precSize) y ys
    in go 0 0 sizes


--------------------------------------------------------------------------------
-- | A generic implementation of 'sizeOf'.
gsizeOf :: forall a. HasLayout a => a -> Int
gsizeOf _ = layoutSize (layout (Proxy :: Proxy a))


--------------------------------------------------------------------------------
-- | A generic implementation of 'peek'.
gpeek :: forall a xs. (All2 Storable (Code a), Code a ~ '[xs], Generic a, HasLayout a, SingI xs) => Ptr a -> IO a
gpeek ptr =
  case layoutOffsets (layout (Proxy :: Proxy a)) of
    POP (offsets :* _) ->
      fmap (to . SOP . Z) $
      hsequence (hcliftA storable
                         (\(K info) ->
                           peek (castPtr $ ptr `plusPtr` layoutFieldOffset info))
                         offsets)


--------------------------------------------------------------------------------
-- | A generic implementation of 'poke'.
gpoke :: forall a xs. (All2 Storable (Code a), Code a ~ '[xs], Generic a, HasLayout a, SingI xs) => Ptr a -> a -> IO ()
gpoke ptr (from -> SOP (Z np)) =
  case layoutOffsets (layout (Proxy :: Proxy a)) of
    POP (offsets :* _) ->
      void $
      hsequenceK
        (hcliftA2 storable
                  (\(I x) (K info) ->
                     K (poke (castPtr $ ptr `plusPtr`
                              fromIntegral (layoutFieldOffset info))
                             x))
                  np
                  offsets)
gpoke _ _ = error "Generics.SOP.Storable.gpoke: Unreachable code... reached? Please report this as a bug."
