module Flow(module Flow, module Data.Function) where

import Data.Function

infixl 2 &.
(&.) :: (a -> b) -> (b -> c) -> a -> c
(&.) = flip (.)
{-# INLINE (&.) #-}

infixl 3 &.>
(&.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
(&.>) g f = fmap f . g
{-# INLINE (&.>) #-}

infixl 1 &>
(&>) :: Functor f => f a -> (a -> b) -> f b
(&>) = flip (<$>)
{-# INLINE (&>) #-}

infixl 1 &>>
(&>>) :: (Functor f, Functor g) => g (f a) -> (a -> b) -> g (f b)
(&>>) l f = l &> (&> f)
{-# INLINE (&>>) #-}

-- infixl 1 &!>
-- (&!>) :: [a] -> (a -> b) -> [b]
-- (&!>) l a = parMap rseq a l
-- {-# INLINE (&!>) #-}