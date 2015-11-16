{-# LANGUAGE GADTs, RankNTypes #-}
module Data.Exist.Trans where

import Data.Functor.Identity


-- |
-- Hide the last parameter of a type constructor.
-- 
--   >>> let x = Some (Just 3) :: Some Maybe
--   >>> let y = Some (Right 3) :: Some (Either String)
-- 
-- Since the type information is lost, we can't print values of that type
-- nor do anything else with those values. Here we fmap those values to ()
-- in order to show the surrounding structure.
-- 
--   >>> let fmapU = fmap (const ())
--   >>> unSome fmapU x
--   Just ()
--   >>> unSome fmapU y
--   Right ()
-- 
-- Note that the existential quantifier is outside of the type constructor.
-- In particular `Some []` corresponds to `exists a. [a]`, not to `[exists a. a]`,
-- and so is not a heterogenous list.
-- 
--   -- type mismatch
--   -- >>> Some [(), "foo"] :: Some []
--   
--   >>> unSome fmapU (Some [3, 4] :: Some [])
--   [(),()]
-- 
-- To approximate `exists a. a`, use `Some Identity`.
-- 
--   >>> :{
--   let xs :: [Some Identity]
--       xs = [ Some (Identity 3)
--            , Some (Identity "foo")
--            ]
--   in fmap (unSome (runIdentity . fmapU)) xs
--   :}
--   [(),()]
data Some f where
    Some :: f a -> Some f

unSome :: (forall a. f a -> r) -> Some f -> r
unSome cc (Some x) = cc x
