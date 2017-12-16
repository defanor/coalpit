{- |
Description :  Command-line options parsing and printing
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

Coalpit is a library for building "command-line program interfaces":
the goal is to get interfaces between programs quickly and easily,
while keeping them language-agnostic and more user- and shell
scripting-friendly than JSON and similar formats.


== Example

@
\{\-\# LANGUAGE DeriveGeneric \#\-\}
import GHC.Generics
import Data.Proxy
import System.Environment
import Coalpit

data Foo = Foo { bar :: Maybe Int
               , baz :: String
               } deriving (Generic, Show)
instance 'ArgParser' Foo
instance 'ToArgs' Foo
instance 'ArgHelper' Foo

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
@

Then, in a shell:

> $ ./Example 'a string'
> Foo {bar = Nothing, baz = "a string"}
> ["a string"]
> $ ./Example --bar 42 'a string'
> Foo {bar = Just 42, baz = "a string"}
> ["--bar","42","a string"]
> $ ./Example --bar foo
> arguments:1:3:
> Failed to read: foo
>
> Usage: [--bar INT] STRING

-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coalpit (
  -- * Core classes
  ArgParser(..)
  , ToArgs(..)
  , ArgHelper(..)
  -- * Utility functions
  , fromArgs
  , usage
  -- * Options
  , Options(..)
  , defOpt
  -- * Parsing helpers
  , Parser
  , CLArg(..)
  , pS
  , readArg
  ) where

import Data.List
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char
import Data.Proxy
import Data.Semigroup
import Data.Void
import qualified Data.List.NonEmpty as NE


-- | Command-line argument wrapper, used to avoid orphan ShowToken
-- String and Stream [String] instances.
newtype CLArg = CLArg { unArg :: String }
  deriving (Ord, Eq)

-- | Advances by one token.
advance :: Pos -> SourcePos -> t -> SourcePos
advance _ (SourcePos n l c) _ = SourcePos n l (c <> pos1)

-- | A list of strings (command-line arguments) stream.
instance Stream [CLArg] where
  type Token [CLArg] = CLArg
  type Tokens [CLArg] = [CLArg]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = advance
  advanceN Proxy w = foldl' (advance w)
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ([], s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span

instance ShowToken CLArg where
  showTokens xs = concat $ NE.map unArg xs

-- | Command-line arguments parser.
type Parser = Parsec Void [CLArg]

-- | Applies a String parser to a single argument.
pS :: Parsec Void String a -> Parsec Void [CLArg] a
pS p = try $ do
  x <- token (Right . unArg) Nothing
  case parse p "argument" x of
    Left e -> fail $ show e
    Right x' -> pure x'


-- | Printing and parsing options.
data Options = Options { conNameMod :: String -> String
                       -- ^ Constructor name modifier.
                       , selNameMod :: String -> String
                       -- ^ Record selector name modifier.
                       , alwaysUseSelName :: Bool
                       -- ^ Add record selector name always, not just
                       -- for optional arguments.
                       , omitNamedOptions :: Bool
                       -- ^ Omit named Maybe values to indicate
                       -- 'Nothing'.
                       }

-- | Default options.
defOpt :: Options
defOpt = Options (map toLower) (("--" ++) . map toLower) False True


-- Core classes

-- | Arguments parser class.
class ArgParser a where
  argParser :: Options -> Parser a
  default argParser :: (Generic a, GArgParser (Rep a)) => Options -> Parser a
  argParser o = to <$> gArgParser o

class GArgParser f where
  gArgParser :: Options -> Parser (f a)

-- | Parses arguments.
fromArgs :: ArgParser a => Options -> [String] -> Either String a
fromArgs o args = case parse (argParser o) "arguments" (map CLArg args) of
  Left err -> Left $ parseErrorPretty err
  Right x -> Right x

-- | Arguments serializer class.
class ToArgs a where
  toArgs :: Options -> a -> [String]
  default toArgs :: (Generic a, GToArgs (Rep a)) => Options -> a -> [String]
  toArgs o a = gToArgs o (from a)

class GToArgs f where
  gToArgs :: Options -> f a -> [String]

-- | Helper class.
class ArgHelper a where
  argHelper :: Options -> [String] -> Proxy a -> String
  default argHelper :: (GArgHelper (Rep a))
                    => Options -> [String] -> Proxy a -> String
  argHelper o path Proxy = gArgHelper o path (Proxy :: Proxy (Rep a f))

class GArgHelper f where
  gArgHelper :: Options -> [String] -> Proxy (f a) -> String

-- | Composes a usage string.
usage :: ArgHelper a => Options -> Proxy a -> String
usage o = argHelper o []

-- Units

instance GArgParser U1 where
  gArgParser _ = pure U1

instance GToArgs U1 where
  gToArgs _ U1 = []

instance GArgHelper U1 where
  gArgHelper _ _ (Proxy :: Proxy (U1 f)) = ""

-- Products

instance (GArgParser a, GArgParser b) => GArgParser (a :*: b) where
  gArgParser m = (:*:) <$> gArgParser m <*> gArgParser m

instance (GToArgs a, GToArgs b) => GToArgs (a :*: b) where
  gToArgs m (a :*: b) = gToArgs m a ++ gToArgs m b

instance (GArgHelper a, GArgHelper b) => GArgHelper (a :*: b) where
  gArgHelper m path (Proxy :: Proxy ((a :*: b) f)) =
    concat [ gArgHelper m path (Proxy :: Proxy (a f))
           , " "
           , gArgHelper m path (Proxy :: Proxy (b f))]


-- Sums

instance (Constructor c1, GArgParser f1, GArgParser (f :+: g)) =>
  GArgParser ((f :+: g) :+: C1 c1 f1) where
  gArgParser m =
    L1 <$> gArgParser m
    <|>
    R1 <$> (pS (string (conNameMod m $ conName (undefined :: C1 c1 f a)))
            *> gArgParser m)

instance (Constructor c1, GArgParser f1, GArgParser (f :+: g)) =>
  GArgParser (C1 c1 f1 :+: (f :+: g)) where
  gArgParser m =
    L1 <$> (pS (string (conNameMod m $ conName (undefined :: C1 c1 f a)))
            *> gArgParser m)
    <|>
    R1 <$> gArgParser m

instance (Constructor c1, Constructor c2, GArgParser f1, GArgParser f2) =>
  GArgParser (C1 c1 f1 :+: C1 c2 f2) where
  gArgParser m =
    L1 <$> (pS (string (conNameMod m $ conName (undefined :: C1 c1 f a)))
            *> gArgParser m)
    <|>
    R1 <$> (pS (string (conNameMod m $ conName (undefined :: C1 c2 f a)))
            *> gArgParser m)

instance (Constructor c1, GToArgs f1, GToArgs (f :+: g)) =>
  GToArgs ((f :+: g) :+: C1 c1 f1) where
  gToArgs m (L1 x) = gToArgs m x
  gToArgs m (R1 x) = conNameMod m (conName x) : gToArgs m x

instance (Constructor c1, GToArgs f1, GToArgs (f :+: g)) =>
  GToArgs (C1 c1 f1 :+: (f :+: g)) where
  gToArgs m (L1 x) = conNameMod m (conName x) : gToArgs m x
  gToArgs m (R1 x) = gToArgs m x

instance (Constructor c1, Constructor c2, GToArgs f1, GToArgs f2) =>
  GToArgs (C1 c1 f1 :+: C1 c2 f2) where
  gToArgs m (L1 x) = conNameMod m (conName x) : gToArgs m x
  gToArgs m (R1 x) = conNameMod m (conName x) : gToArgs m x

spaceNonEmpty :: String -> String
spaceNonEmpty "" = ""
spaceNonEmpty s = ' ' : s

instance (Constructor c1, GArgHelper f1, GArgHelper (f :+: g)) =>
  GArgHelper ((f :+: g) :+: C1 c1 f1) where
  gArgHelper m path (Proxy :: Proxy (((f :+: g) :+: C1 c1 f1) p)) =
    let cName1 = conName (undefined :: C1 c1 f a)
    in concat [ "("
              , gArgHelper m path (Proxy :: Proxy ((f :+: g) p))
              , " | "
              , conNameMod m cName1
              , if cName1 `elem` path
                then "..."
                else spaceNonEmpty $
                     gArgHelper m (cName1 : path) (Proxy :: Proxy (f1 p))
              , ")"]

instance (Constructor c1, GArgHelper f1, GArgHelper (f :+: g)) =>
  GArgHelper (C1 c1 f1 :+: (f :+: g)) where
  gArgHelper m path (Proxy :: Proxy ((C1 c1 f1 :+: (f :+: g)) p)) =
    let cName1 = conName (undefined :: C1 c1 f a)
    in concat [ "("
              , conNameMod m cName1
              , if cName1 `elem` path
                then "..."
                else spaceNonEmpty $
                     gArgHelper m (cName1 : path) (Proxy :: Proxy (f1 p))
              , " | "
              , gArgHelper m path (Proxy :: Proxy ((f :+: g) p))
              , ")"]

instance (Constructor c1, Constructor c2, GArgHelper f1, GArgHelper f2) =>
  GArgHelper (C1 c1 f1 :+: C1 c2 f2) where
  gArgHelper m path (Proxy :: Proxy ((C1 c1 f1 :+: C1 c2 f2) p)) =
    let cName1 = conName (undefined :: C1 c1 f a)
        cName2 = conName (undefined :: C1 c2 f a)
    in concat [ "("
              , conNameMod m cName1
              , if cName1 `elem` path
                then "..."
                else spaceNonEmpty $
                     gArgHelper m (cName1 : path) (Proxy :: Proxy (f1 p))
              , " | "
              , conNameMod m cName2
              , if cName2 `elem` path
                then "..."
                else spaceNonEmpty $
                     gArgHelper m (cName2 : path) (Proxy :: Proxy (f2 p))
              , ")"]


-- Record Selectors

parseS1 :: (GArgParser a) => String -> Options -> Parser (S1 c a f)
parseS1 n o =
  let sname = case (n, alwaysUseSelName o) of
        ("", _) -> pure ()
        (_, False) -> pure ()
        (name, True) -> pS (string (selNameMod o name)) >> pure ()
  in M1 <$> (sname *> gArgParser o)

printS1 :: (GToArgs f, Selector c) => Options -> S1 c f a -> [String]
printS1 o s@(M1 x) = case (selName s, alwaysUseSelName o) of
                       ("", _) -> gToArgs o x
                       (_, False) -> gToArgs o x
                       (name, True) -> selNameMod o name : gToArgs o x

helpS1 :: (GArgHelper a)
       => String -> Options -> [String] -> Proxy (S1 c a f) -> String
helpS1 n o path (Proxy :: Proxy ((S1 c a) f)) =
  case (n, alwaysUseSelName o) of
    ("", _) -> gArgHelper o path (Proxy :: Proxy (a f))
    (_, False) -> gArgHelper o path (Proxy :: Proxy (a f))
    (name, True) -> concat [ selNameMod o name
                           , " "
                           , gArgHelper o path (Proxy :: Proxy (a f))]

instance (GArgParser a, Selector c) => GArgParser (S1 c a) where
  gArgParser = parseS1 (selName (undefined :: S1 c a f))

instance (GToArgs a, Selector c) => GToArgs (S1 c a) where
  gToArgs = printS1

instance (GArgHelper a, Selector c) => GArgHelper (S1 c a) where
  gArgHelper = helpS1 (selName (undefined :: S1 c a f))


-- Optional arguments

instance {-#OVERLAPPING#-}
  (ArgParser a, Selector c) => GArgParser (S1 c (Rec0 (Maybe a))) where
  gArgParser m =
    let n = selName (undefined :: S1 c (Rec0 (Maybe a)) f)
    in case (omitNamedOptions m, null n) of
      (True, True) -> M1 <$> gArgParser m
      (True, False) ->
        M1 . K1 <$> optional (pS (string (selNameMod m n)) *> argParser m)
      _ -> parseS1 n m

instance {-#OVERLAPPING#-}
  (ToArgs a, Selector c) => GToArgs (S1 c (Rec0 (Maybe a))) where
  gToArgs m s@(M1 (K1 x))
    | omitNamedOptions m = case (selName s, x) of
        ("", _) -> toArgs m x
        (_, Nothing) -> []
        (name, Just x') -> selNameMod m name : toArgs m x'
    | otherwise = printS1 m s

instance {-#OVERLAPPING#-}
  (ArgHelper a, Selector c) => GArgHelper (S1 c (Rec0 (Maybe a))) where
  gArgHelper m path (Proxy :: Proxy (S1 c (Rec0 (Maybe a)) f)) =
    let n = selName (undefined :: S1 c (Rec0 (Maybe a)) f)
    in case (omitNamedOptions m, null n) of
      (True, True) -> gArgHelper m path (Proxy :: Proxy (Rec0 (Maybe a) f))
      (True, False) -> concat [ "["
                              , selNameMod m n
                              , " "
                              , gArgHelper m path (Proxy :: Proxy (Rec0 a f))
                              , "]"]
      _ -> helpS1 n m path (Proxy :: Proxy (S1 c (Rec0 (Maybe a)) f))


-- Constructors

instance (GArgParser a) => GArgParser (C1 c a) where
  gArgParser m = M1 <$> gArgParser m

instance (GToArgs a) => GToArgs (C1 c a) where
  gToArgs m (M1 x) = gToArgs m x

instance (GArgHelper a) => GArgHelper (C1 c a) where
  gArgHelper m path (Proxy :: Proxy (C1 c a f)) =
    gArgHelper m path (Proxy :: Proxy (a f))

-- Data types

instance (GArgParser a) => GArgParser (D1 c a) where
  gArgParser m = M1 <$> gArgParser m

instance (GToArgs a) => GToArgs (D1 c a) where
  gToArgs m (M1 x) = gToArgs m x

instance (GArgHelper a) => GArgHelper (D1 c a) where
  gArgHelper m path (Proxy :: Proxy (D1 c a f)) =
    gArgHelper m path (Proxy :: Proxy (a f))

-- Constraints and such

instance (ArgParser a) => GArgParser (K1 i a) where
  gArgParser m = K1 <$> argParser m

instance (ToArgs a) => GToArgs (K1 i a) where
  gToArgs m (K1 x) = toArgs m x

instance (ArgHelper a) => GArgHelper (K1 i a) where
  gArgHelper m path (Proxy :: Proxy (K1 x a f)) =
    argHelper m path (Proxy :: Proxy a)


-- Common types

-- | Reads an argument using its 'Read' instance.
readArg :: Read a => Parser a
readArg = do
  x <- token (Right . unArg) Nothing
  case reads x of
    [(n, "")] -> pure n
    _ -> fail $ "Failed to read: " ++ x

instance ArgParser Int where
  argParser _ = readArg
instance ToArgs Int where
  toArgs _ i = [show i]
instance ArgHelper Int where
  argHelper _ _ _ = "INT"

instance ArgParser Integer where
  argParser _ = readArg
instance ToArgs Integer where
  toArgs _ i = [show i]
instance ArgHelper Integer where
  argHelper _ _ _ = "INTEGER"

instance ArgParser Rational where
  argParser _ = readArg
instance ToArgs Rational where
  toArgs _ i = [show i]
instance ArgHelper Rational where
  argHelper _ _ _ = "RATIONAL"

instance ArgParser Double where
  argParser _ = readArg
instance ToArgs Double where
  toArgs _ i = [show i]
instance ArgHelper Double where
  argHelper _ _ _ = "DOUBLE"

instance {-#OVERLAPPING#-} ArgParser String where
  argParser _ = token (Right . unArg) Nothing
instance {-#OVERLAPPING#-} ToArgs String where
  toArgs _ i = [i]
instance {-#OVERLAPPING#-} ArgHelper String where
  argHelper _ _ _ = "STRING"

instance ArgParser Bool
instance ToArgs Bool
instance ArgHelper Bool

instance ArgParser a => ArgParser (Maybe a)
instance ToArgs a => ToArgs (Maybe a)
instance ArgHelper a => ArgHelper (Maybe a)

instance ArgParser a => ArgParser [a]
instance ToArgs a => ToArgs [a]
instance ArgHelper a => ArgHelper [a]

instance (ArgParser a, ArgParser b) => ArgParser (Either a b)
instance (ToArgs a, ToArgs b) => ToArgs (Either a b)
instance (ArgHelper a, ArgHelper b) => ArgHelper (Either a b)

-- | Expects a dot.
instance ArgParser () where
  argParser _ = pS (char '.') *> pure ()
-- | Shows a dot.
instance ToArgs () where
  toArgs _ () = ["."]
instance ArgHelper () where
  argHelper _ _ _ = "."

instance (ArgParser a, ArgParser b) => ArgParser (a, b)
instance (ToArgs a, ToArgs b) => ToArgs (a, b)
instance (ArgHelper a, ArgHelper b) => ArgHelper (a, b)

instance (ArgParser a, ArgParser b, ArgParser c) => ArgParser (a, b, c)
instance (ToArgs a, ToArgs b, ToArgs c) => ToArgs (a, b, c)
instance (ArgHelper a, ArgHelper b, ArgHelper c) => ArgHelper (a, b, c)

instance (ArgParser a, ArgParser b, ArgParser c, ArgParser d) =>
  ArgParser (a, b, c, d)
instance (ToArgs a, ToArgs b, ToArgs c, ToArgs d) => ToArgs (a, b, c, d)
instance (ArgHelper a, ArgHelper b, ArgHelper c, ArgHelper d) =>
  ArgHelper (a, b, c, d)

instance (ArgParser a, ArgParser b, ArgParser c, ArgParser d, ArgParser e) =>
  ArgParser (a, b, c, d, e)
instance (ToArgs a, ToArgs b, ToArgs c, ToArgs d, ToArgs e) =>
  ToArgs (a, b, c, d, e)
instance (ArgHelper a, ArgHelper b, ArgHelper c, ArgHelper d, ArgHelper e) =>
  ArgHelper (a, b, c, d, e)
