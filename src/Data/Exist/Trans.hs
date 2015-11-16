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
--   >>> :{
--   let x :: Some Maybe
--       x = Some (Just 3)
--   in unSome blankFunctor x
--   :}
--   Just ()
--   
--   >>> :{
--   let x :: Some (Either String)
--       x = Some (Right 3)
--   in unSome blankFunctor x
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
--   >>> unSome blankFunctor (Some [3, 4] :: Some [])
--   [(),()]
-- 
-- To approximate `exists a. a`, use `Some Identity`:
-- 
--   >>> :{
--   let xs :: [Some Identity]
--       xs = [ Some (Identity 3)
--            , Some (Identity "foo")
--            ]
--   in fmap (unSome (runIdentity . blankFunctor)) xs
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
--   let mkFoo :: Either a b -> EitherXX
--       mkFoo = SomeT . Some
--       --
--       unFoo :: (forall a b. Either a b -> r) -> EitherXX -> r
--       unFoo cc = unSomeT $ unSome $ cc
--       --
--       myFoo :: [EitherXX]
--       myFoo = [mkFoo (Left 1), mkFoo (Left "foo"), mkFoo (Right 42)]
--   in map (unFoo blankBifunctor) myFoo
--   :}
--   [Left (),Left (),Right ()]
data SomeT t f where
    SomeT :: t (f a) -> SomeT t f

unSomeT :: (forall a. t (f a) -> r) -> SomeT t f -> r
unSomeT cc (SomeT x) = cc x
