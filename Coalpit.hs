{- |
Description :  Command-line options and DSV parsing and printing
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

Coalpit is a library for building "command-line program interfaces":
the goal is to get interfaces between programs quickly and easily,
while keeping them language-agnostic and more user- and shell
scripting-friendly than JSON and similar formats.


== Example

@
\{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass \#\-\}
import GHC.Generics
import Data.Proxy
import System.Environment
import Coalpit

data Foo = Foo { bar :: Maybe Int
               , baz :: String
               } deriving (Show, Generic, 'Coalpit')

main :: IO ()
main = do
  args <- getArgs
  case 'fromArgs' 'defOpt' args of
    Left err -> do
      putStrLn err
      putStrLn $ "Usage: " ++ 'usage' 'defOpt' (Proxy :: Proxy Foo)
    Right x -> do
      print (x :: Foo)
      print $ 'toArgs' 'defOpt' x
      putStr $ 'showDSV' 'defOpt' [x]
      print ('readDSV' 'defOpt' $ 'showDSV' 'defOpt' [x] :: [Either String Foo])
@

Then, in a shell:

> $ ./Example 'a string'
> Foo {bar = Nothing, baz = "a string"}
> ["a string"]
> "a string"
> Right (Foo {bar = Nothing, baz = "a string"})
> $ ./Example --bar 42 'a string'
> Foo {bar = Just 42, baz = "a string"}
> ["--bar","42","a string"]
> --bar 42 "a string"
> Right (Foo {bar = Just 42, baz = "a string"})
> $ ./Example --bar foo
> arguments:1:3:
> Failed to read: foo
>
> Usage: [--bar INT] STRING

-}

module Coalpit ( module Coalpit.Core
               , module Coalpit.DSV
               ) where

import Coalpit.Core
import Coalpit.DSV
