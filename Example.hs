{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Text.Megaparsec
import Coalpit

data FooArgs = FooArgs { arg1 :: Int
                       , arg2 :: String
                       } deriving (Generic, Show)
instance ArgParser FooArgs
instance ToArgs FooArgs

data FooBar = Foo FooArgs
            | Bar
            deriving (Generic, Show)
instance ArgParser FooBar
instance ToArgs FooBar

data Input = Input { something :: Maybe String
                   , fooBar :: Maybe FooBar
                   , fooBar2 :: FooBar
                   } deriving (Generic, Show)
instance ArgParser Input
instance ToArgs Input

main :: IO ()
main = do
  let val = Input { something = Nothing
                  , fooBar = Just (Foo (FooArgs { arg1 = 1
                                                , arg2 = "a string"}))
                  , fooBar2 = Bar}
      args = toArgs defMod val
  print val
  print args
  print $ parse (argParser defMod :: Parser Input) "test" args
