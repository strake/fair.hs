module Data.List.Fair (Fair (..)) where

import Control.Applicative
import Data.Foldable
import Data.Functor.Classes
import Numeric.Natural
import Util

newtype Fair a = Fair { unFair :: [a] }
  deriving (Functor, Foldable, Traversable, Eq, Ord, Read, Show, Eq1, Ord1, Read1, Show1)

instance Applicative Fair where
    pure a = Fair [a]
    fs <*> xs = fs >>= (<$> xs)

instance Monad Fair where
    Fair xs >>= f = Fair $ [0..] >>= flip diag (unFair . f <$> xs)

diag :: Natural -> [[a]] -> [a]
diag _ [] = []
diag 0 _  = []
diag k (as:ass) = toList (as !!? k) ++ diag (k-1) ass

instance Semigroup a => Semigroup (Fair a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (Fair a) where
    mempty = pure mempty
