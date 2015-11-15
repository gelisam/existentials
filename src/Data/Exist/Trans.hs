{-# LANGUAGE GADTs #-}
module Data.Exist.Trans where

import Data.Functor.Identity


-- |
-- Hide the last parameter of a type constructor.
-- 
--   >>> let x = Some (Just 3) :: Some Maybe
--   >>> let y = Some (Right 3) :: Some (Either String)
-- 
-- Since the type information is lost, we can't print values of that type
-- nor do anything else with those values.
-- 
--   >>> :{
--   case x of
--     Some (Just _) -> "Just _"
--     Some Nothing  -> show Nothing
--   :}
--   "Just _"
--   
--   >>> :{
--   case y of
--     Some (Left  s) -> show (Left s)
--     Some (Right _) -> "Right _"
--   :}
--   "Right _"
-- 
-- Note that the existential quantifier is outside of the type constructor.
-- In particular `Some []` corresponds to `exists a. [a]`, not to `[exists a. a]`,
-- and so is not a heterogenous list.
-- 
--   -- type mismatch
--   -- >>> let xs = Some [3, "foo"] :: Some []
--   
--   >>> let x = Some [3, 4] :: Some []
--   >>> :{
--   case x of
--     Some xs -> length xs
--   :}
--   2
-- 
-- To approximate `exists a. a`, use `Some Identity`.
-- 
--   >>> :{
--   let ys = [ Some (Identity 3)
--            , Some (Identity "foo")
--            ] :: [Some Identity]
--    in length ys
--   :}
--   2
data Some f where
    Some :: f a -> Some f
