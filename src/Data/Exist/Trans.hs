{-# LANGUAGE GADTs, PolyKinds, RankNTypes #-}
module Data.Exist.Trans where

import Data.Functor.Identity

-- $setup
-- >>> :set -XRankNTypes
-- >>> import Data.Bifunctor
-- 
-- Since existential quantifiers hide the type information, we can't print
-- values of an existentially-quantified type nor do anything else with those
-- values. In this file's examples, we'll blank out those values in order to
-- show the surrounding structure.
-- 
-- >>> :{
-- let blank :: a -> ()
--     blank = const ()
--     --
--     blankFunctor :: Functor f => f a -> f ()
--     blankFunctor = fmap blank
--     --
--     blankBifunctor :: Bifunctor f => f a b -> f () ()
--     blankBifunctor = bimap blank blank
-- :}

-- |
-- Existentially-quantify over the last parameter of a type constructor.
-- 
-- Here are `exists a. Maybe a` and `exists a. Either String a`:
-- 
--   >>> type MaybeX = Some Maybe
--   >>> :{
--   let mkMaybeX :: Maybe a -> MaybeX
--       mkMaybeX = Some
--       --
--       unMaybeX :: (forall a. Maybe a -> r) -> MaybeX -> r
--       unMaybeX cc = unSome $ cc
--       --
--       myMaybeX :: MaybeX
--       myMaybeX = mkMaybeX (Just 3)
--   in unMaybeX blankFunctor myMaybeX
--   :}
--   Just ()
--   
--   >>> type EitherStringX = Some (Either String)
--   >>> :{
--   let mkEitherStringX :: Either String a -> EitherStringX
--       mkEitherStringX = Some
--       --
--       unEitherStringX :: (forall a. Either String a -> r) -> EitherStringX -> r
--       unEitherStringX cc = unSome $ cc
--       --
--       myEitherStringX :: EitherStringX
--       myEitherStringX = mkEitherStringX (Right 3)
--   in unEitherStringX blankFunctor myEitherStringX
--   :}
--   Right ()
-- 
-- Note that the existential quantifier is outside of the type constructor.
-- In particular `Some []` corresponds to `exists a. [a]`, not to `[exists a. a]`,
-- and so is not a heterogenous list.
-- 
--   -- type mismatch
--   -- >>> Some [(), "foo"] :: Some []
--   
--   >>> type ListX = Some []
--   >>> :{
--   let mkListX :: [a] -> ListX
--       mkListX = Some
--       --
--       unListX :: (forall a. [a] -> r) -> ListX -> r
--       unListX cc = unSome $ cc
--       --
--       myListX :: ListX
--       myListX = mkListX [3, 4]
--   in unListX blankFunctor myListX
--   :}
--   [(),()]
-- 
-- To approximate `exists a. a`, use `Some Identity`:
-- 
--   >>> type X = Some Identity
--   >>> :{
--   let mkX :: a -> X
--       mkX = Some . Identity
--       --
--       unX :: (forall a. a -> r) -> X -> r
--       unX cc = unSome (cc . runIdentity)
--       --
--       myXs :: [X]
--       myXs = [ mkX 3
--              , mkX "foo"
--              ]
--   in fmap (unX blank) myXs
--   :}
--   [(),()]
data Some f where
    Some :: f a -> Some f

unSome :: (forall a. f a -> r) -> Some f -> r
unSome cc (Some x) = cc x


-- |
-- Transformer variant of `Some`.
-- 
-- This allows nested quantifiers. Here's `exists a b. Either a b`:
-- 
--   >>> type EitherXX = SomeT Some Either
--   >>> :{
--   let mkEitherXX :: Either a b -> EitherXX
--       mkEitherXX = SomeT . Some
--       --
--       unEitherXX :: (forall a b. Either a b -> r) -> EitherXX -> r
--       unEitherXX cc = unSomeT $ unSome $ cc
--       --
--       myEitherXXs :: [EitherXX]
--       myEitherXXs = [ mkEitherXX (Left 1)
--                     , mkEitherXX (Left "foo")
--                     , mkEitherXX (Right 42)
--                     ]
--   in map (unEitherXX blankBifunctor) myEitherXXs
--   :}
--   [Left (),Left (),Right ()]
data SomeT t f where
    SomeT :: t (f a) -> SomeT t f

unSomeT :: (forall a. t (f a) -> r) -> SomeT t f -> r
unSomeT cc (SomeT x) = cc x
