{-# LANGUAGE GADTs #-}
module Data.Exist.Trans where

-- |
-- Hide the last argument of a type constructor.
-- Since the type information is lost, we can't print values of that type
-- nor do anything else with those values.
-- 
-- >>> let x = Some (Just 3) :: Some Maybe
-- >>> :{
-- case x of
--   Some (Just _) -> "Just _"
--   Some Nothing  -> show Nothing
-- :}
-- "Just _"
-- 
-- >>> let x = Some (Right 3) :: Some (Either String)
-- >>> :{
-- case x of
--   Some (Left  s) -> show (Left s)
--   Some (Right _) -> "Right _"
-- :}
-- "Right _"
data Some f where
    Some :: f a -> Some f
