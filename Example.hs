{-# LANGUAGE TemplateHaskell #-}

module Main where

import Coalpit
import Language.Haskell.TH
import Data.List

data Y = Foo Bool Int
       | Bar Int
       | Baz
  deriving (Show)

data X = X String (Maybe Int) (Maybe [Int]) Y Y String
  deriving (Show)

$(deriveArgs ''Y)
$(deriveArgs ''X)

main :: IO ()
main = do
  let val = X "test" Nothing (Just [1,2,3]) (Foo True 1) Baz "end"
      args = toArgs val
  print val
  putStrLn $ intercalate " " args
  print (fromArgs args :: (X, [String]))
  print (fromArgs (args ++ ["additional", "args"]) :: (X, [String]))
