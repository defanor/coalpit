{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main where

import GHC.Generics
import Coalpit
import Data.Proxy

data FooArgs = FooArgs { arg1 :: Int
                       , arg2 :: String
                       } deriving (Show, Generic, Coalpit)

data FooBar = Foo FooArgs
            | Bar
            deriving (Show, Generic, Coalpit)

data Input = Input { something :: Maybe String
                   , fooBar :: Maybe FooBar
                   , fooBar2 :: FooBar
                   } deriving (Show, Generic, Coalpit)

main :: IO ()
main = do
  let val = Input { something = Nothing
                  , fooBar = Just (Foo FooArgs { arg1 = 1
                                               , arg2 = "a string"})
                  , fooBar2 = Bar}
      dsv = toDSV defOpt val
  print val
  print dsv
  print (fromDSV defOpt dsv :: Either String Input)

data Test = Test { foo :: [Int], bar :: Maybe String }
  deriving (Show, Generic, Coalpit)

help :: IO ()
help = do
  mapM_ (\(o, x, y) -> print o >> putStrLn x >> putStrLn y) $
    [ let opts = defOpt { selNamePolicy = snpol }
      in ( snpol
         , toDSV opts (Test [1,2,3] vals)
         , usageString opts (Proxy :: Proxy Test))
      | snpol <- [SNDisable, SNAvoid, SNPrefer, SNRequire]
      , vals <- [Just "a string", Nothing]]
