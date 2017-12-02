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

instance ToArgs Foo
instance ArgParser Foo

main :: IO ()
main = do
  let val = Qux Nothing (Just 1) (RecTest Nothing (Just 2.3) Nothing) Nothing
      a = args val
  print val
  putStrLn a
  print $ parse (argParser :: Parser Foo) "test" a
