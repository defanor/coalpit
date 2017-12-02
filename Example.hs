{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics
import Text.Megaparsec
import Coalpit

data RecTest = RecTest { a :: Int, b :: Double }
             deriving (Generic, Show)

instance ArgParser RecTest
instance ToArgs RecTest

data Foo = Bar Int
         | Baz Int
         | Qux RecTest
  deriving (Generic, Show)

instance ToArgs Foo
instance ArgParser Foo

main :: IO ()
main = do
  let val = Qux (RecTest 1 2.3)
      a = args val
  print val
  putStrLn a
  print $ parse (argParser :: Parser Foo) "test" a
