{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Contravariant

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) :: (a->b) -> Compose f g a -> Compose f g b
  (<$>) h (Compose k)= Compose ((h<$>)<$>k)

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure
  (<*>) :: Compose f g (a->b) -> Compose f g a -> Compose f g b
  (<*>) (Compose h) (Compose k) = Compose $ lift2 (<*>) h k

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
  (=<<) :: (a -> Compose f g b) -> Compose f g a -> Compose f g b
  (=<<) = undefined

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
instance (Functor f, Contravariant g) =>
  Contravariant (Compose f g) where
-- Implement the (>$<) function for a Contravariant instance for Compose
  (>$<) ::
    (b -> a)
    -> Compose f g a
    -> Compose f g b
  (>$<) h (Compose k) =  Compose ((h >$<)<$>k)