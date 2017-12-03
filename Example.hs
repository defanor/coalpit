{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Text.Megaparsec
import Coalpit

data RecTest = RecTest { a :: Maybe Int
                       , b :: Maybe Double
                       , c :: Maybe Int }
             deriving (Generic, Show)

instance ArgParser RecTest
instance ToArgs RecTest

data Foo = Bar Int
         | Baz Int
         | Qux [Int] (Maybe Int) (Either String Int) RecTest (Maybe Double)
  deriving (Generic, Show)

instance ArgParser Foo
instance ToArgs Foo

data Wrap = Wrap { foo :: Maybe Foo, num :: Maybe Int }
  deriving (Generic, Show)

instance ArgParser Wrap
instance ToArgs Wrap

main :: IO ()
main = do
  let val = Wrap (Just $ Qux [1,2,3] Nothing (Left "foo bar")
                  (RecTest Nothing (Just 2.3) Nothing) Nothing) (Just 1)
      a = toArgs defMod val
  print val
  print a
  print $ parse (argParser defMod :: Parser Wrap) "test" a
