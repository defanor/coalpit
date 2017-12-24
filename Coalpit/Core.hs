{- |
Module      :  Coalpit.Core
Description :  Core Coalpit definitions
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

The 'Coalpit' class with instances, a few functions to work with it,
and 'Options' are defined here.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coalpit.Core ( Coalpit(..)
                    , fromArgs
                    -- * Usage
                    , Usage(..)
                    , usage
                    , usageString
                    -- * Options
                    , Options(..)
                    , defOpt
                    ) where

import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Char (toLower)
import Data.Proxy (Proxy(..))
import qualified Data.List.NonEmpty as NE
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time.Clock (DiffTime, NominalDiffTime, UniversalTime, UTCTime)
import Data.Time.Format ( TimeLocale, formatTime
                        , iso8601DateFormat, defaultTimeLocale)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay, LocalTime, ZonedTime)
import Data.Scientific (Scientific, FPFormat(..), formatScientific, scientificP)
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Complex (Complex)
import Data.Version (Version, parseVersion, showVersion)
import System.Exit (ExitCode)
import Network.URI (URI, parseURIReference, uriToString)

import Coalpit.Parsing

data Usage = UConstructor String
           | URecursive String
           | USelector String Usage
           | UOptional Usage
           | USum Usage Usage
           | UProduct Usage Usage
           | UUnit
           | UType String

-- | Printing and parsing options.
data Options = Options { fieldSeparator :: Char
                       -- ^ DSV field separator ('showDSV',
                       -- 'readDSV').
                       , conNameMod :: String -> String
                       -- ^ Constructor name modifier.
                       , selNameMod :: String -> String
                       -- ^ Record selector name modifier.
                       , alwaysUseSelName :: Bool
                       -- ^ Add record selector name always, not just
                       -- for optional arguments.
                       , omitNamedOptions :: Bool
                       -- ^ Omit named Maybe values to indicate
                       -- 'Nothing'.
                       , timeLocale :: TimeLocale
                       , dateFormat :: String
                       -- ^ See "Data.Time.Format".
                       , timeFormat :: String
                       , dateTimeFormat :: String
                       , scientificFormat :: FPFormat
                       , scientificDecimals :: Maybe Int
                       , uriUserInfo :: String -> String
                       -- ^ Used to map userinfo parts of URIs.
                       }

-- | Default options.
defOpt :: Options
defOpt = Options ' ' (map toLower) (("--" ++) . map toLower) False True
  defaultTimeLocale (iso8601DateFormat Nothing) "%H:%M:%S"
  (iso8601DateFormat (Just "%H:%M:%S")) Generic Nothing id

-- | Parses arguments.
fromArgs :: Coalpit a => Options -> [String] -> Either String a
fromArgs opt args = case parse (argParser opt) "arguments" (map CLArg args) of
  Left err -> Left $ parseErrorPretty err
  Right x -> Right x

-- | Composes 'Usage' description.
usage :: Coalpit a => Options -> Proxy a -> Usage
usage opt = argHelper opt []

-- | Composes a usage string.
usageString :: Coalpit a => Options -> Proxy a -> String
usageString opt = usageToString . usage opt

-- | Translates 'Usage' into a string, used by 'usageString'.
usageToString :: Usage -> String
usageToString (UConstructor c) = c
usageToString (URecursive c) = c ++ "..."
usageToString (USelector s u) = s ++ " " ++ usageToString u
usageToString (UOptional u) = "[" ++ usageToString u ++ "]"
usageToString (USum ul ur) = concat [ "("
                                    , usageToString ul
                                    , " | "
                                    , usageToString ur
                                    , ")"]
usageToString (UProduct u1 UUnit) = usageToString u1
usageToString (UProduct u1 u2) = usageToString u1 ++ " " ++ usageToString u2
usageToString UUnit = ""
usageToString (UType t) = t


-- | Coalpit class: parsing, printing, usage strings.
class Coalpit a where
  argParser :: Options -> Parser a
  default argParser :: (Generic a, GCoalpit (Rep a)) => Options -> Parser a
  argParser opt = to <$> gArgParser opt

  toArgs :: Options -> a -> [String]
  default toArgs :: (Generic a, GCoalpit (Rep a)) => Options -> a -> [String]
  toArgs opt a = gToArgs opt (from a)

  argHelper :: Options -> [String] -> Proxy a -> Usage
  default argHelper :: (GCoalpit (Rep a))
                    => Options -> [String] -> Proxy a -> Usage
  argHelper opt path Proxy = gArgHelper opt path (Proxy :: Proxy (Rep a p))

class GCoalpit a where
  gArgParser :: Options -> Parser (a p)
  gToArgs :: Options -> a p -> [String]
  gArgHelper :: Options -> [String] -> Proxy (a p) -> Usage


-- Units
instance GCoalpit U1 where
  gArgParser _ = pure U1
  gToArgs _ U1 = []
  gArgHelper _ _ (Proxy :: Proxy (U1 f)) = UUnit


-- Products
instance (GCoalpit a, GCoalpit b) => GCoalpit (a :*: b) where
  gArgParser opt = (:*:) <$> gArgParser opt <*> gArgParser opt
  gToArgs opt (x :*: y) = gToArgs opt x ++ gToArgs opt y
  gArgHelper opt path (Proxy :: Proxy ((a :*: b) p)) =
    UProduct (gArgHelper opt path (Proxy :: Proxy (a p)))
    (gArgHelper opt path (Proxy :: Proxy (b p)))


-- Sums

-- | Handles recursive constructors.
handleRecCon :: GCoalpit a
             => String
             -- ^ Constructor name
             -> Options
             -> [String]
             -> Proxy (a p)
             -> Usage
handleRecCon nameA opt path (Proxy :: Proxy (a p)) =
  let n = conNameMod opt nameA
  in if nameA `elem` path
     then URecursive n
     else UProduct (UConstructor n)
          (gArgHelper opt (nameA : path) (Proxy :: Proxy (a p)))

instance (Constructor conA, GCoalpit a, GCoalpit (b :+: c)) =>
  GCoalpit ((b :+: c) :+: C1 conA a) where
  gArgParser opt =
    L1 <$> gArgParser opt
    <|>
    R1 <$> (pS (string (conNameMod opt $ conName (undefined :: C1 conA a p)))
            *> gArgParser opt)
  gToArgs opt (L1 x) = gToArgs opt x
  gToArgs opt (R1 x) = conNameMod opt (conName x) : gToArgs opt x
  gArgHelper opt path (Proxy :: Proxy (((b :+: c) :+: C1 conA a) p)) =
    let nameA = conName (undefined :: C1 conA f p)
    in USum (gArgHelper opt path (Proxy :: Proxy ((b :+: c) p)))
       (handleRecCon nameA opt path (Proxy :: Proxy (a p)))

instance (Constructor conA, GCoalpit a, GCoalpit (b :+: c)) =>
  GCoalpit (C1 conA a :+: (b :+: c)) where
  gArgParser opt =
    L1 <$> (pS (string (conNameMod opt $ conName (undefined :: C1 conA a p)))
            *> gArgParser opt)
    <|>
    R1 <$> gArgParser opt
  gToArgs opt (L1 x) = conNameMod opt (conName x) : gToArgs opt x
  gToArgs opt (R1 x) = gToArgs opt x
  gArgHelper opt path (Proxy :: Proxy ((C1 conA a :+: (b :+: c)) p)) =
    let nameA = conName (undefined :: C1 conA a p)
    in USum (handleRecCon nameA opt path (Proxy :: Proxy (a p)))
       (gArgHelper opt path (Proxy :: Proxy ((b :+: c) p)))

instance (Constructor conA, Constructor conB, GCoalpit a, GCoalpit b) =>
  GCoalpit (C1 conA a :+: C1 conB b) where
  gArgParser opt =
    L1 <$> (pS (string (conNameMod opt $
                        conName (undefined :: C1 conA a p)))
            *> gArgParser opt)
    <|>
    R1 <$> (pS (string (conNameMod opt $
                        conName (undefined :: C1 conB b p)))
            *> gArgParser opt)
  gToArgs opt (L1 x) = conNameMod opt (conName x) : gToArgs opt x
  gToArgs opt (R1 x) = conNameMod opt (conName x) : gToArgs opt x
  gArgHelper opt path (Proxy :: Proxy ((C1 conA a :+: C1 conB b) p)) =
    let nameA = conName (undefined :: C1 conA a p)
        nameB = conName (undefined :: C1 conB b p)
    in USum (handleRecCon nameA opt path (Proxy :: Proxy (a p)))
       (handleRecCon nameB opt path (Proxy :: Proxy (b p)))


-- Record Selectors

parseS1 :: (GCoalpit a) => String -> Options -> Parser (S1 selA a p)
parseS1 nameA opt =
  let sName = case (nameA, alwaysUseSelName opt) of
        ("", _) -> pure ()
        (_, False) -> pure ()
        (_, True) -> pS (string (selNameMod opt nameA)) >> pure ()
  in M1 <$> (sName *> gArgParser opt)

printS1 :: (GCoalpit a, Selector selA) => Options -> S1 selA a p -> [String]
printS1 opt sel@(M1 x) = case (selName sel, alwaysUseSelName opt) of
                           ("", _) -> gToArgs opt x
                           (_, False) -> gToArgs opt x
                           (name, True) -> selNameMod opt name : gToArgs opt x

helpS1 :: (GCoalpit a)
       => String -> Options -> [String] -> Proxy (S1 selA a p) -> Usage
helpS1 nameA opt path (Proxy :: Proxy (S1 selA a p)) =
  case (nameA, alwaysUseSelName opt) of
    ("", _) -> gArgHelper opt path (Proxy :: Proxy (a p))
    (_, False) -> gArgHelper opt path (Proxy :: Proxy (a p))
    (_, True) -> USelector (selNameMod opt nameA)
      (gArgHelper opt path (Proxy :: Proxy (a p)))

instance (GCoalpit a, Selector selA) => GCoalpit (S1 selA a) where
  gArgParser = parseS1 (selName (undefined :: S1 selA a p))
  gToArgs = printS1
  gArgHelper = helpS1 (selName (undefined :: S1 selA a p))

-- Optional arguments
instance {-#OVERLAPPING#-}
  (Coalpit a, Coalpit (Maybe a), Selector selA) =>
  GCoalpit (S1 selA (Rec0 (Maybe a))) where
  gArgParser opt =
    let nameA = selName (undefined :: S1 selA (Rec0 (Maybe a)) p)
    in case (omitNamedOptions opt, null nameA) of
      (True, True) -> M1 <$> gArgParser opt
      (True, False) ->
        M1 . K1
        <$> optional (pS (string (selNameMod opt nameA)) *> argParser opt)
      _ -> parseS1 nameA opt
  gToArgs opt sel@(M1 (K1 x))
    | omitNamedOptions opt = case (selName sel, x) of
        ("", _) -> toArgs opt x
        (_, Nothing) -> []
        (nameA, Just x') -> selNameMod opt nameA : toArgs opt x'
    | otherwise = printS1 opt sel
  gArgHelper opt path (Proxy :: Proxy (S1 selA (Rec0 (Maybe a)) p)) =
    let nameA = selName (undefined :: S1 selA (Rec0 (Maybe a)) p)
    in case (omitNamedOptions opt, null nameA) of
      (True, True) -> gArgHelper opt path (Proxy :: Proxy (Rec0 (Maybe a) p))
      (True, False) -> UOptional $ USelector (selNameMod opt nameA)
                       (gArgHelper opt path (Proxy :: Proxy (Rec0 a p)))
      _ -> helpS1 nameA opt path (Proxy :: Proxy (S1 selA (Rec0 (Maybe a)) p))


-- Constructors

instance (GCoalpit a) => GCoalpit (C1 conA a) where
  gArgParser = fmap M1 . gArgParser
  gToArgs opt (M1 x) = gToArgs opt x
  gArgHelper opt path (Proxy :: Proxy (C1 conA a p)) =
    gArgHelper opt path (Proxy :: Proxy (a p))

-- Data types
instance (GCoalpit a) => GCoalpit (D1 conA a) where
  gArgParser = fmap M1 . gArgParser
  gToArgs opt (M1 x) = gToArgs opt x
  gArgHelper opt path (Proxy :: Proxy (D1 conA a p)) =
    gArgHelper opt path (Proxy :: Proxy (a p))

-- Constraints and such
instance (Coalpit a) => GCoalpit (K1 i a) where
  gArgParser = fmap K1 . argParser
  gToArgs opt (K1 x) = toArgs opt x
  gArgHelper opt path (Proxy :: Proxy (K1 x a p)) =
    argHelper opt path (Proxy :: Proxy a)


-- Common types

instance Coalpit Int where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "INT"

instance Coalpit Integer where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "INTEGER"

instance Coalpit Word8 where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "WORD8"

instance Coalpit Word16 where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "WORD16"

instance Coalpit Word32 where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "WORD32"

instance Coalpit Word64 where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "WORD64"

instance Coalpit Int8 where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "INT8"

instance Coalpit Int16 where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "INT16"

instance Coalpit Int32 where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "INT32"

instance Coalpit Int64 where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "INT64"

instance Coalpit Natural where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "NATURAL"

instance Coalpit Rational where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "RATIONAL"

instance Coalpit Double where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "DOUBLE"

instance Coalpit Float where
  argParser _ = readArg
  toArgs _ i = [show i]
  argHelper _ _ _ = UType "FLOAT"

instance Coalpit Char where
  argParser _ = readArg
  toArgs _ c = [show c]
  argHelper _ _ _ = UType "CHAR"

instance {-#OVERLAPPING#-} Coalpit String where
  argParser _ = token (Right . unArg) Nothing
  toArgs _ i = [i]
  argHelper _ _ _ = UType "STRING"

instance Coalpit Scientific where
  argParser _ = try $ do
    x <- token (Right . unArg) Nothing
    case reverse $ readP_to_S scientificP x of
      (n, ""):_ -> pure n
      _ -> fail $ "Failed to read a scientific number: " ++ x
  toArgs opt n = [formatScientific
                  (scientificFormat opt) (scientificDecimals opt) n]
  argHelper _ _ _ = UType "SCIENTIFIC"

instance Coalpit Version where
  argParser _ = try $ do
    x <- token (Right . unArg) Nothing
    case reverse $ readP_to_S parseVersion x of
      (v, ""):_ -> pure v
      _ -> fail $ "Failed to read a version: " ++ x
  toArgs _ v = [showVersion v]
  argHelper _ _ _ = UType "VERSION"

-- | An URI reference (absolute or relative).
instance Coalpit URI where
  argParser _ = try $ do
    x <- token (Right . unArg) Nothing
    maybe (fail $ "Failed to parse URI: " ++ x) pure (parseURIReference x)
  toArgs opt u = [uriToString (uriUserInfo opt) u ""]
  argHelper _ _ _ = UType "URI"


-- | Uses 'dateTimeFormat'.
instance Coalpit UTCTime where
  argParser opt = pTime (timeLocale opt) (dateTimeFormat opt)
  toArgs opt t = [formatTime (timeLocale opt) (dateTimeFormat opt) t]
  argHelper _ _ _ = UType "UTC_TIME"

-- | Uses 'dateTimeFormat'.
instance Coalpit ZonedTime where
  argParser opt = pTime (timeLocale opt) (dateTimeFormat opt)
  toArgs opt t = [formatTime (timeLocale opt) (dateTimeFormat opt) t]
  argHelper _ _ _ = UType "ZONED_TIME"

-- | Uses 'dateTimeFormat'.
instance Coalpit LocalTime where
  argParser opt = pTime (timeLocale opt) (dateTimeFormat opt)
  toArgs opt t = [formatTime (timeLocale opt) (dateTimeFormat opt) t]
  argHelper _ _ _ = UType "LOCAL_TIME"

-- | Uses 'dateTimeFormat'.
instance Coalpit UniversalTime where
  argParser opt = pTime (timeLocale opt) (dateTimeFormat opt)
  toArgs opt t = [formatTime (timeLocale opt) (dateTimeFormat opt) t]
  argHelper _ _ _ = UType "UNIVERSAL_TIME"

-- | Uses 'timeFormat'.
instance Coalpit TimeOfDay where
  argParser opt = pTime (timeLocale opt) (timeFormat opt)
  toArgs opt t = [formatTime (timeLocale opt) (timeFormat opt) t]
  argHelper _ _ _ = UType "TIME_OF_DAY"

-- | Uses 'dateFormat'.
instance Coalpit Day where
  argParser opt = pTime (timeLocale opt) (dateFormat opt)
  toArgs opt t = [formatTime (timeLocale opt) (dateFormat opt) t]
  argHelper _ _ _ = UType "DAY"

-- | Converts to/from 'Scientific'.
instance Coalpit NominalDiffTime where
  argParser opt = fromRational . toRational
                  <$> (argParser opt :: Parser Scientific)
  toArgs opt = toArgs opt .
    (fromRational . toRational :: NominalDiffTime -> Scientific)
  argHelper _ _ _ = UType "NOMINAL_DIFF_TIME"

-- | Converts to/from 'Scientific'.
instance Coalpit DiffTime where
  argParser opt = fromRational . toRational
                  <$> (argParser opt :: Parser Scientific)
  toArgs opt = toArgs opt .
    (fromRational . toRational :: DiffTime -> Scientific)
  argHelper _ _ _ = UType "DIFF_TIME"


instance Coalpit ()
instance Coalpit Bool
instance Coalpit Ordering
instance Coalpit ExitCode
instance Coalpit a => Coalpit (Complex a)
instance Coalpit a => Coalpit (Maybe a)
instance Coalpit a => Coalpit [a]
instance Coalpit a => Coalpit (NE.NonEmpty a)
instance (Coalpit a, Coalpit b) => Coalpit (Either a b)
instance (Coalpit a, Coalpit b) => Coalpit (a, b)
instance (Coalpit a, Coalpit b, Coalpit c) => Coalpit (a, b, c)
instance (Coalpit a, Coalpit b, Coalpit c, Coalpit d) => Coalpit (a, b, c, d)
