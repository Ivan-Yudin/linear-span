{-#  LANGUAGE  DeriveFunctor           #-}
{-#  LANGUAGE  FlexibleInstances       #-}
{-#  LANGUAGE  FunctionalDependencies  #-}
{-#  LANGUAGE  MultiParamTypeClasses   #-}

module Data.LS (LS(..),simplify,(.*)) where

import Prelude hiding (head)

import Control.Monad
import Control.Applicative

import Data.Function (on)
import Data.List (sortBy)
import Data.List.NonEmpty (head, groupBy)

-- | Linear  spans of elements in @a@ with  coefficients in @c@.
data LS c a = LS {unLS :: [Term c a ]}  deriving (Functor,Eq,Ord)
-- where
data Term c t = Term {coeff :: c, term :: t}
              deriving (Eq, Functor,  Ord)




-- | Simplifies the expression
simplify :: (Ord a, Num c, Eq c) => LS c a -> LS c a
simplify (LS x) = LS $
              filter ( (/=0) . coeff)  $
              fmap ( \y -> Term ( sum $ fmap coeff y) (term $ head y)) $
              groupBy ( (==) `on` term ) $
              reverse  $
              sortBy ( compare `on` term ) x

-- | A bit artificial but useful class.
class Times coeff a | a -> coeff  where
  (.*) :: coeff -> a -> a

-- | Various instances for linear spans
instance (Num c ) => Times c (LS c a) where
  k .* (LS x) = LS $ [ Term (k*c) t | Term c t <- x ]

instance (Num c) => Applicative (LS c) where
  pure a = LS [ Term 1 a ]
  (<*> ) = ap

instance (Num coef) => Monad (LS coef) where
    (LS terms) >>= h = LS $ do
        Term c  t  <- terms
        Term c' t' <- unLS $ h t
        pure $ Term (c*c') t'

instance (Num c) => Alternative (LS c) where
   empty = LS []
   (LS x) <|> (LS y) = LS (x <|> y)

instance Semigroup (LS c a) where
   (LS x) <> (LS y) = LS (x <> y)

instance Monoid (LS c a) where
   mempty = LS []
