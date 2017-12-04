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
import System.Environment
import Coalpit

data Foo = Foo { bar :: Maybe Int
               , baz :: String
               } deriving (Generic, Show)
instance 'ArgParser' Foo
instance 'ToArgs' Foo

main :: IO ()
main = do
  args <- getArgs
  case 'fromArgs' 'defMod' args of
    Left err -> putStrLn err
    Right x -> do
      print (x :: Foo)
      print $ 'toArgs' 'defMod' x
@

Then, in a shell:

> $ ./Example 'a string'
> Foo {bar = Nothing, baz = "a string"}
> ["a string"]
> $ ./Example --bar 42 'a string'
> Foo {bar = Just 42, baz = "a string"}
> ["--bar","42","a string"]

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
  -- * Utility functions
  , fromArgs
  -- * Modifiers
  , Modifiers(..)
  , defMod
  -- * Parsing helpers
  , Parser
  , Args(..)
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


-- | Command-line arguments wrapper, used to avoid an orphan 'Stream'
-- instance.
newtype Args = Args [String]

-- | Advances by one token.
advance :: Pos -> SourcePos -> t -> SourcePos
advance _ (SourcePos n l c) _ = SourcePos n l (c <> pos1)

-- | A list of strings (command-line arguments) stream.
instance Stream Args where
  type Token Args = String
  type Tokens Args = [String]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = advance
  advanceN Proxy w = foldl' (advance w)
  take1_ (Args []) = Nothing
  take1_ (Args (t:ts)) = Just (t, Args ts)
  takeN_ n (Args s)
    | n <= 0    = Just ([], Args s)
    | null s    = Nothing
    | otherwise = Just (take n s, Args (drop n s))
  takeWhile_ f (Args s) = (takeWhile f s, Args (dropWhile f s))

-- | Command-line arguments parser.
type Parser = Parsec String Args

-- | Applies a String parser to a single argument.
pS :: Parsec String String a -> Parsec String Args a
pS p = try $ do
  x <- token Right Nothing
  case parse p "argument" x of
    Left e -> fail $ show e
    Right x' -> pure x'


-- | Name modifiers.
data Modifiers = Modifiers { conNameMod :: String -> String
                           -- ^ Constructor name modifier.
                           , selNameMod :: String -> String
                           -- ^ Record selector name modifier.
                           , alwaysUseSelName :: Bool
                           -- ^ Add record selector name always, not
                           -- just for optional arguments.
                           }

-- | Default modifiers.
defMod :: Modifiers
defMod = Modifiers (map toLower) (("--" ++) . map toLower) False


-- Core classes

-- | Arguments parser class.
class ArgParser a where
  argParser :: Modifiers -> Parser a
  default argParser :: (Generic a, GArgParser (Rep a)) => Modifiers -> Parser a
  argParser m = to <$> gArgParser m

class GArgParser f where
  gArgParser :: Modifiers -> Parser (f a)

-- | Parses arguments.
fromArgs :: ArgParser a => Modifiers -> [String] -> Either String a
fromArgs m args = case parse (argParser m) "arguments" (Args args) of
  Left err -> Left $ show err
  Right x -> Right x

-- | Arguments serializer class.
class ToArgs a where
  toArgs :: Modifiers -> a -> [String]
  default toArgs :: (Generic a, GToArgs (Rep a)) => Modifiers -> a -> [String]
  toArgs m a = gToArgs m (from a)

class GToArgs f where
  gToArgs :: Modifiers -> f a -> [String]


-- Units

instance GArgParser U1 where
  gArgParser _ = pure U1

instance GToArgs U1 where
  gToArgs _ U1 = []


-- Products

instance (GArgParser a, GArgParser b) => GArgParser (a :*: b) where
  gArgParser m = (:*:) <$> gArgParser m <*> gArgParser m

instance (GToArgs a, GToArgs b) => GToArgs (a :*: b) where
  gToArgs m (a :*: b) = gToArgs m a ++ gToArgs m b


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


-- Record Selectors

instance (GArgParser a, Selector c) => GArgParser (S1 c a) where
  gArgParser m = M1 <$> do
    let sname = case (selName (undefined :: S1 c a f), alwaysUseSelName m) of
          ("", _) -> pure ()
          (_, False) -> pure ()
          (name, True) -> pS (string (selNameMod m name)) >> pure ()
    sname *> gArgParser m

instance (GToArgs a, Selector c) => GToArgs (S1 c a) where
  gToArgs m s@(M1 x) = case (selName s, alwaysUseSelName m) of
                         ("", _) -> gToArgs m x
                         (_, False) -> gToArgs m x
                         (name, True) -> selNameMod m name : gToArgs m x


-- Optional arguments

instance {-#OVERLAPPING#-}
  (ArgParser a, Selector c) => GArgParser (S1 c (Rec0 (Maybe a))) where
  gArgParser m =
    case selName (undefined :: S1 c (Rec0 (Maybe a)) f) of
      "" -> M1 <$> gArgParser m
      name -> do
        x <- optional $ pS (string (selNameMod m name)) *> argParser m
        pure $ M1 $ K1 x

instance {-#OVERLAPPING#-}
  (ToArgs a, Selector c) => GToArgs (S1 c (Rec0 (Maybe a))) where
  gToArgs m s@(M1 (K1 x)) = case (selName s, x) of
    ("", _) -> toArgs m x
    (_, Nothing) -> []
    (name, Just x') -> selNameMod m name : toArgs m x'


-- Constructors

instance (GArgParser a) => GArgParser (C1 c a) where
  gArgParser m = M1 <$> gArgParser m

instance (GToArgs a) => GToArgs (C1 c a) where
  gToArgs m (M1 x) = gToArgs m x


-- Data types

instance (GArgParser a) => GArgParser (D1 c a) where
  gArgParser m = M1 <$> gArgParser m

instance (GToArgs a) => GToArgs (D1 c a) where
  gToArgs m (M1 x) = gToArgs m x


-- Constraints and such

instance (ArgParser a) => GArgParser (K1 i a) where
  gArgParser m = K1 <$> argParser m

instance (ToArgs a) => GToArgs (K1 i a) where
  gToArgs m (K1 x) = toArgs m x


-- Common types

-- | Reads an argument using its 'Read' instance.
readArg :: Read a => Parser a
readArg = do
  x <- token Right Nothing
  case reads x of
    [(n, "")] -> pure n
    _ -> fail $ "Failed to read: " ++ x

instance ArgParser Int where
  argParser _ = readArg
instance ToArgs Int where
  toArgs _ i = [show i]

instance ArgParser Integer where
  argParser _ = readArg
instance ToArgs Integer where
  toArgs _ i = [show i]

instance ArgParser Rational where
  argParser _ = readArg
instance ToArgs Rational where
  toArgs _ i = [show i]

instance ArgParser Double where
  argParser _ = readArg
instance ToArgs Double where
  toArgs _ i = [show i]

instance {-#OVERLAPPING#-} ArgParser String where
  argParser _ = token Right Nothing
instance {-#OVERLAPPING#-} ToArgs String where
  toArgs _ i = [i]

instance ArgParser Bool
instance ToArgs Bool

instance ArgParser a => ArgParser (Maybe a)
instance ToArgs a => ToArgs (Maybe a)

instance ArgParser a => ArgParser [a]
instance ToArgs a => ToArgs [a]

instance (ArgParser a, ArgParser b) => ArgParser (Either a b)
instance (ToArgs a, ToArgs b) => ToArgs (Either a b)

-- | Expects a dot.
instance ArgParser () where
  argParser _ = pS (char '.') *> pure ()
-- | Shows a dot.
instance ToArgs () where
  toArgs _ () = ["."]

instance (ArgParser a, ArgParser b) => ArgParser (a, b)
instance (ToArgs a, ToArgs b) => ToArgs (a, b)

instance (ArgParser a, ArgParser b, ArgParser c) => ArgParser (a, b, c)
instance (ToArgs a, ToArgs b, ToArgs c) => ToArgs (a, b, c)

instance (ArgParser a, ArgParser b, ArgParser c, ArgParser d) =>
  ArgParser (a, b, c, d)
instance (ToArgs a, ToArgs b, ToArgs c, ToArgs d) => ToArgs (a, b, c, d)

instance (ArgParser a, ArgParser b, ArgParser c, ArgParser d, ArgParser e) =>
  ArgParser (a, b, c, d, e)
instance (ToArgs a, ToArgs b, ToArgs c, ToArgs d, ToArgs e) =>
  ToArgs (a, b, c, d, e)
