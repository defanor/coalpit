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

type Parser = Parsec String String

args :: ToArgs a => a -> String
args = intercalate " " . toArgs

class ArgParser a where
  argParser :: Parser a
  default argParser :: (Generic a, GArgParser (Rep a)) => Parser a
  argParser = to <$> gArgParser

class GArgParser f where
  gArgParser :: Parser (f a)

class ToArgs a where
  toArgs :: a -> [String]
  default toArgs :: (Generic a, GToArgs (Rep a)) => a -> [String]
  toArgs a = gToArgs (from a)

class GToArgs f where
  gToArgs :: f a -> [String]


instance GArgParser U1 where
  gArgParser = pure U1

instance GToArgs U1 where
  gToArgs U1 = []

instance (GArgParser a, GArgParser b) => GArgParser (a :*: b) where
  gArgParser = (:*:) <$> gArgParser <* space <*> gArgParser

instance (GToArgs a, GToArgs b) => GToArgs (a :*: b) where
  gToArgs (a :*: b) = gToArgs a ++ gToArgs b

instance (Constructor c1, GArgParser f1, GArgParser (f :+: g)) =>
  GArgParser ((f :+: g) :+: C1 c1 f1) where
  gArgParser =
    L1 <$> gArgParser
    <|>
    R1 <$> (string (conName (undefined :: C1 c1 f a)) *> space *> gArgParser)

instance (Constructor c1, GArgParser f1, GArgParser (f :+: g)) =>
  GArgParser (C1 c1 f1 :+: (f :+: g)) where
  gArgParser =
    L1 <$> (string (conName (undefined :: C1 c1 f a)) *> space *> gArgParser)
    <|>
    R1 <$> gArgParser

instance (Constructor c1, Constructor c2, GArgParser f1, GArgParser f2) =>
  GArgParser (C1 c1 f1 :+: C1 c2 f2) where
  gArgParser =
    L1 <$> (string (conName (undefined :: C1 c1 f a)) *> space *> gArgParser)
    <|>
    R1 <$> (string (conName (undefined :: C1 c2 f a)) *> space *> gArgParser)

instance (Constructor c1, GToArgs f1, GToArgs (f :+: g)) =>
  GToArgs ((f :+: g) :+: C1 c1 f1) where
  gToArgs (L1 x) = gToArgs x
  gToArgs (R1 x) = conName x : gToArgs x

instance (Constructor c1, GToArgs f1, GToArgs (f :+: g)) =>
  GToArgs (C1 c1 f1 :+: (f :+: g)) where
  gToArgs (L1 x) = conName x : gToArgs x
  gToArgs (R1 x) = gToArgs x

instance (Constructor c1, Constructor c2, GToArgs f1, GToArgs f2) =>
  GToArgs (C1 c1 f1 :+: C1 c2 f2) where
  gToArgs (L1 x) = conName x : gToArgs x
  gToArgs (R1 x) = conName x : gToArgs x

instance {-#OVERLAPPING#-}
  (ArgParser a, Selector c) => GArgParser (S1 c (Rec0 (Maybe a))) where
  gArgParser = do
    case selName (undefined :: S1 c (Rec0 (Maybe a)) f) of
           "" -> M1 <$> gArgParser
           name -> do
             x <- optional $ string ("--" ++ name) *> space *> argParser
             pure $ M1 $ K1 x

instance (GArgParser a, Selector c) => GArgParser (S1 c a) where
  gArgParser = M1 <$> do
    let sname = case selName (undefined :: S1 c a f) of
          "" -> pure ()
          name -> string ("--" ++ name) *> space
    sname *> gArgParser

-- record selectors
instance {-#OVERLAPPING#-}
  (ToArgs a, Selector c) => GToArgs (S1 c (Rec0 (Maybe a))) where
  gToArgs s@(M1 (K1 x)) = case (selName s, x) of
                       ("", _) -> toArgs x
                       (_, Nothing) -> []
                       (name, Just x') -> ("--" ++ selName s) : toArgs x'

instance (GToArgs a, Selector c) => GToArgs (S1 c a) where
  gToArgs s@(M1 x) = case selName s of
                         "" -> gToArgs x
                         name -> ("--" ++ name) : gToArgs x

instance (GArgParser a, Constructor c) => GArgParser (C1 c a) where
  gArgParser = M1 <$> gArgParser

instance (GToArgs a, Constructor c) => GToArgs (C1 c a) where
  gToArgs c@(M1 x) = gToArgs x


instance (GArgParser a, Datatype c) => GArgParser (D1 c a) where
  gArgParser = M1 <$> gArgParser

instance (GToArgs a, Datatype c) => GToArgs (D1 c a) where
  gToArgs d@(M1 x) = gToArgs x

instance (ArgParser a) => GArgParser (K1 i a) where
  gArgParser = K1 <$> argParser

instance (ToArgs a) => GToArgs (K1 i a) where
  gToArgs (K1 x) = toArgs x


instance ArgParser Int where
  argParser = read <$> some digitChar

instance ToArgs Int where
  toArgs i = [show i]

instance ArgParser String where
  argParser = char '"' *> manyTill anyChar (char '"')

instance ToArgs String where
  toArgs i = [show i]

instance ArgParser Double where
  argParser = read <$> some (digitChar <|> char '.')

instance ToArgs Double where
  toArgs i = [show i]

instance ArgParser a => ArgParser (Maybe a)
instance ToArgs a => ToArgs (Maybe a)
