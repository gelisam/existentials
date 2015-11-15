module Data.Exist.Trans.Test where

import Test.DocTest


-- |
-- >>> 2+2
-- 4
test :: IO ()
test = doctest [ "src/Data/Exist/Trans/Test.hs"
               , "src/Data/Exist/Trans.hs"
               ]
