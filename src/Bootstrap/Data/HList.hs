{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Description : Defines a type for heterogeneous lists.
-- Copyright : (c) Crown Copyright GCHQ
module Bootstrap.Data.HList (HList (..), (~:)) where

-- | Represents a heterogeneous list (a list where the type of
-- each element can be different).
data HList :: [Type] -> Type where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

infixr 5 ~:

-- | Operator alias for `HCons`.
(~:) :: x -> HList xs -> HList (x ': xs)
(~:) = HCons
