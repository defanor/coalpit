{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Coalpit
import Data.Proxy

data FooArgs = FooArgs { arg1 :: Int
                       , arg2 :: String
                       } deriving (Generic, Show)
instance ArgParser FooArgs
instance ToArgs FooArgs
instance ArgHelper FooArgs

data FooBar = Foo FooArgs
            | Bar
            deriving (Generic, Show)
instance ArgParser FooBar
instance ToArgs FooBar
instance ArgHelper FooBar

data Input = Input { something :: Maybe String
                   , fooBar :: Maybe FooBar
                   , fooBar2 :: FooBar
                   } deriving (Generic, Show)
instance ArgParser Input
instance ToArgs Input
instance ArgHelper Input

main :: IO ()
main = do
  let val = Input { something = Nothing
                  , fooBar = Just (Foo FooArgs { arg1 = 1
                                               , arg2 = "a string"})
                  , fooBar2 = Bar}
      args = toArgs defOpt val
  print val
  print args
  print (fromArgs defOpt args :: Either String Input)

data Test = Test { foo :: [Int], bar :: Maybe String }
  deriving (Generic, Show)

instance ArgParser Test
instance ToArgs Test
instance ArgHelper Test

help :: IO ()
help = do
  mapM_ (\(o, x, y) -> print o >> print x >> putStrLn y) $
    [ let opts = defOpt { alwaysUseSelName = ausn
                        , omitNamedOptions = ono }
      in ((ausn, ono), toArgs opts (Test [] vals), argHelper opts [] (Proxy :: Proxy Test))
      | ausn <- [True, False]
      , ono <- [True, False]
      , vals <- [Just "a string", Nothing]]
