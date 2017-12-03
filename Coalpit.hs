{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coalpit where

import Data.List
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe
import Data.Char

type Parser = Parsec String String

args :: ToArgs a => Modifiers -> a -> String
args m = intercalate " " . toArgs m


data Modifiers = Modifiers { conNameMod :: String -> String
                           , selNameMod :: String -> String }

defMod :: Modifiers
defMod = Modifiers (map toLower) (("--" ++) . map toLower)

-- Core classes

class ArgParser a where
  argParser :: Modifiers -> Parser a
  default argParser :: (Generic a, GArgParser (Rep a)) => Modifiers -> Parser a
  argParser m = to <$> gArgParser m

class GArgParser f where
  gArgParser :: Modifiers -> Parser (f a)

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
  gArgParser m = (:*:) <$> gArgParser m <* space <*> gArgParser m

instance (GToArgs a, GToArgs b) => GToArgs (a :*: b) where
  gToArgs m (a :*: b) = gToArgs m a ++ gToArgs m b


-- Sums

instance (Constructor c1, GArgParser f1, GArgParser (f :+: g)) =>
  GArgParser ((f :+: g) :+: C1 c1 f1) where
  gArgParser m =
    L1 <$> gArgParser m
    <|>
    R1 <$> (string (conNameMod m $ conName (undefined :: C1 c1 f a))
            *> space *> gArgParser m)

instance (Constructor c1, GArgParser f1, GArgParser (f :+: g)) =>
  GArgParser (C1 c1 f1 :+: (f :+: g)) where
  gArgParser m =
    L1 <$> (string (conNameMod m $ conName (undefined :: C1 c1 f a))
            *> space *> gArgParser m)
    <|>
    R1 <$> gArgParser m

instance (Constructor c1, Constructor c2, GArgParser f1, GArgParser f2) =>
  GArgParser (C1 c1 f1 :+: C1 c2 f2) where
  gArgParser m =
    L1 <$> (string (conNameMod m $ conName (undefined :: C1 c1 f a))
            *> space *> gArgParser m)
    <|>
    R1 <$> (string (conNameMod m $ conName (undefined :: C1 c2 f a))
            *> space *> gArgParser m)

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
    let sname = case selName (undefined :: S1 c a f) of
          "" -> pure ()
          name -> string (selNameMod m name) *> space
    sname *> gArgParser m

instance (GToArgs a, Selector c) => GToArgs (S1 c a) where
  gToArgs m s@(M1 x) = case selName s of
                         "" -> gToArgs m x
                         name -> selNameMod m name : gToArgs m x


-- Optional arguments

instance {-#OVERLAPPING#-}
  (ArgParser a, Selector c) => GArgParser (S1 c (Rec0 (Maybe a))) where
  gArgParser m = do
    case selName (undefined :: S1 c (Rec0 (Maybe a)) f) of
      "" -> M1 <$> gArgParser m
      name -> do
        x <- optional $ string (selNameMod m name) *> space *> argParser m
        pure $ M1 $ K1 x

instance {-#OVERLAPPING#-}
  (ToArgs a, Selector c) => GToArgs (S1 c (Rec0 (Maybe a))) where
  gToArgs m s@(M1 (K1 x)) = case (selName s, x) of
    ("", _) -> toArgs m x
    (_, Nothing) -> []
    (name, Just x') -> selNameMod m name : toArgs m x'


-- Constructors

instance (GArgParser a, Constructor c) => GArgParser (C1 c a) where
  gArgParser m = M1 <$> gArgParser m

instance (GToArgs a, Constructor c) => GToArgs (C1 c a) where
  gToArgs m c@(M1 x) = gToArgs m x


-- Data types

instance (GArgParser a, Datatype c) => GArgParser (D1 c a) where
  gArgParser m = M1 <$> gArgParser m

instance (GToArgs a, Datatype c) => GToArgs (D1 c a) where
  gToArgs m d@(M1 x) = gToArgs m x


-- Constraints and such

instance (ArgParser a) => GArgParser (K1 i a) where
  gArgParser m = K1 <$> argParser m

instance (ToArgs a) => GToArgs (K1 i a) where
  gToArgs m (K1 x) = toArgs m x


-- Basic types

instance ArgParser Int where
  argParser _ = read <$> some digitChar

instance ToArgs Int where
  toArgs _ i = [show i]

instance ArgParser String where
  argParser _ = char '"' *> manyTill anyChar (char '"')

instance ToArgs String where
  toArgs _ i = [show i]

instance ArgParser Double where
  argParser _ = read <$> some (digitChar <|> char '.')

instance ToArgs Double where
  toArgs _ i = [show i]

instance ArgParser a => ArgParser (Maybe a)
instance ToArgs a => ToArgs (Maybe a)
