{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Text.Megaparsec
import Coalpit

data RecTest = RecTest { a :: Maybe Int, b :: Maybe Double, c :: Maybe Int }
             deriving (Generic, Show)

instance ArgParser RecTest
instance ToArgs RecTest

data Foo = Bar Int
         | Baz Int
         | Qux (Maybe Int) (Maybe Int) RecTest (Maybe Double)
  deriving (Generic, Show)

instance ArgParser Foo
instance ToArgs Foo

data Wrap = Wrap { foo :: Maybe Foo, num :: Maybe Int }
  deriving (Generic, Show)

instance ArgParser Wrap
instance ToArgs Wrap

main :: IO ()
main = do
  let val = Wrap (Just $ Qux Nothing (Just 1) (RecTest Nothing (Just 2.3) Nothing) Nothing) (Just 1)
      a = args val
  print val
  putStrLn a
  print $ parse (argParser :: Parser Wrap) "test" a
