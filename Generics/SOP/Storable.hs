{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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

import Control.Applicative
import Control.Monad (void)
import Data.Proxy (Proxy(Proxy))
import Foreign (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import Generics.SOP (All, All2, Code, Generic, I(I), K(K), NP(Nil, (:*)), NS(Z, S),
                     POP(POP), SingI, SOP(SOP), from, hcliftA, hcliftA2,
                     hcollapse, hliftA, hpure, hsequence, hsequenceK, to, unK)

--------------------------------------------------------------------------------
storable :: Proxy Storable
storable = Proxy


--------------------------------------------------------------------------------
-- | Information about how a specific field should be laid out in memory.
data LayoutInfo a = LayoutInfo
  { layoutFieldOffset :: Int
    -- ^ A byte offset from the initial data pointer to this field.
  }


--------------------------------------------------------------------------------
-- | How to tag the occurance of each constructor.
data ConstructorTags code where
  ConstructorTags :: (Eq tag, Storable tag) => LayoutInfo tag -> NP (K tag) code -> ConstructorTags code


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
  { layoutOffsets :: POP LayoutInfo code
    -- ^ A specification of how each field is laid out.
  , layoutSize :: !Int
    -- ^ The overall size in memory that serialization of this data type requires.
  , layoutTags :: Maybe (ConstructorTags code)
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
    , layoutOffsets = POP (hliftA (LayoutInfo . unK) offsets :* Nil)
    , layoutTags = Nothing
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
data Path :: [k] -> * where
  Here :: Path xs
  There :: Path xs -> Path (x ': xs)


--------------------------------------------------------------------------------
-- | A generic implementation of 'peek'.
gpeek :: forall a. (All SingI (Code a), All2 Storable (Code a), Generic a, HasLayout a) => Ptr a -> IO a
gpeek ptr = do
  let l = layout (Proxy :: Proxy a)
  somePath <-
    case layoutTags l of
      Nothing ->
        return Here

      Just (ConstructorTags info tags) -> do
        tag <- peek (castPtr $ ptr `plusPtr` layoutFieldOffset info)
        let findCtor :: forall code tag. Eq tag => tag -> NP (K tag) code -> Path code
            findCtor _ Nil =
              error "Generics.SOP.gpeek: Unknown constructor"
            findCtor t (K x :* xs)
              | x == t     = Here
              | otherwise = There (findCtor t xs)
        return (findCtor tag tags)

  to . SOP <$> go somePath (layoutOffsets l)

  where
  go :: forall code. (All2 Storable code,All SingI code) => Path code -> POP LayoutInfo code -> IO (NS (NP I) code)
  go Here (POP (offsets :* _)) =
    fmap Z $
    hsequence (hcliftA storable
                       (\info ->
                          peek (castPtr $ ptr `plusPtr` layoutFieldOffset info))
                       offsets)
  go (There p) (POP (_ :* offsets)) =
    fmap S $
    go p (POP offsets)

--------------------------------------------------------------------------------
-- | A generic implementation of 'poke'.
gpoke :: forall a. (All SingI (Code a), All2 Storable (Code a), Generic a, HasLayout a) => Ptr a -> a -> IO ()
gpoke ptr (from -> SOP sop) =
  let l = layout (Proxy :: Proxy a)
  in pokeCtor sop
              (layoutOffsets l)
              (layoutTags l)
  where
  pokeCtor :: forall code. (All2 Storable code,All SingI code) => NS (NP I) code -> POP LayoutInfo code -> Maybe (ConstructorTags code) -> IO ()
  pokeCtor (Z np) (POP (offsets :* _)) tags = do
    case tags of
      Just (ConstructorTags ctorInfo (K t :* _)) ->
        poke (castPtr $ ptr `plusPtr` layoutFieldOffset ctorInfo)
             t
      _ -> return ()

    void $
      do
        hsequenceK
          (hcliftA2 storable
                    (\(I x) info ->
                       K (poke (castPtr $ ptr `plusPtr`
                                fromIntegral (layoutFieldOffset info))
                               x))
                    np
                    offsets)
  pokeCtor (S s) (POP (_ :* offsets)) ctors =
    pokeCtor s
             (POP offsets)
             (case ctors of
                Just (ConstructorTags info (_ :* tags)) ->
                  Just (ConstructorTags info tags)
                _ -> Nothing)
  pokeCtor _ _ _ =
    error "Generics.SOP.Storable.gpoke.pokeCtor: Unreachable code reached. Please report this as a bug."
