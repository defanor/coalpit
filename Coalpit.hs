{- |
Description :  Command-line options and DSV parsing and printing
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

Coalpit is a library for building command-line interfaces: the goal is
to build interfaces quickly and easily (by deriving those), while
keeping them language-agnostic and more user- and shell
scripting-friendly than JSON and similar formats.

-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coalpit ( fromDSV
               , fromDSVList
               , toDSV
               , toDSVList
               , dsvFromList
               , Coalpit(..)
                 -- * Usage
               , usage
               , usageString
               , Usage(..)
                 -- * Options
               , SelNamePolicy(..)
               , Options(..)
               , defOpt
                 -- * Parsing and composition helpers
               , escape
               , pString
               , pFieldSep
               , pRecordSep
               ) where

import GHC.Generics
import Text.Parsec
import Text.Parsec.String
import Data.Char (toLower)
import Data.Proxy (Proxy(..))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.List (intercalate)
import Data.Word (Word8, Word16, Word32, Word64)
import Numeric.Natural (Natural)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time.Clock (DiffTime, NominalDiffTime, UniversalTime, UTCTime)
import Data.Time.Format ( TimeLocale, formatTime
                        , iso8601DateFormat, defaultTimeLocale
                        , ParseTime, readSTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime (TimeOfDay, LocalTime, ZonedTime)
import Data.Scientific (Scientific, FPFormat(..), formatScientific, scientificP)
import Text.ParserCombinators.ReadP (readP_to_S)
import Data.Complex (Complex)
import Data.Version (Version, parseVersion, showVersion)
import System.Exit (ExitCode)
import Network.URI (URI, parseURIReference, uriToString)

-- | Usage description: can be translated into help messages or
-- documentation formats.
data Usage = UConstructor String
           -- ^ Data constructor.
           | URecursive String
           -- ^ Constructor of a recursive data structure (its second
           -- appearance in the tree).
           | USelector Bool String Usage
           -- ^ Record selector.
           | UOptional Usage
           -- ^ Optional element.
           | USum Usage Usage
           -- ^ Sum.
           | UProduct Usage Usage
           -- ^ Product.
           | UUnit
           -- ^ Unit.
           | UType String
           -- ^ Type name (e.g., \"INT\").
           deriving (Show)

-- | How to handle selector names.
data SelNamePolicy = SNDisable
                   -- ^ Do not parse or print selector names
                   | SNAvoid
                   -- ^ Allow selector names on parsing, but do not
                   -- print them
                   | SNPrefer
                   -- ^ Allow selector names on parsing, print them
                   | SNRequire
                   -- ^ Require selector names on parsing, print them
  deriving (Show, Eq)

-- | Printing and parsing options.
data Options = Options { fieldSeparators :: NonEmpty Char
                       -- ^ Separators between fields
                       , recordSeparators :: NonEmpty Char
                       -- ^ Separators between records (which may
                       -- correspond to lines)
                       , conNameMod :: String -> String
                       -- ^ Constructor name modifier
                       , selNameMod :: String -> String
                       -- ^ Record selector name modifier
                       , selNamePolicy :: SelNamePolicy
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
defOpt = Options (' ' :| ['\t']) ('\n' :| [])
  (map toLower) (("--" ++) . map toLower) SNAvoid
  defaultTimeLocale (iso8601DateFormat Nothing) "%H:%M:%S"
  (iso8601DateFormat (Just "%H:%M:%S")) Generic Nothing id

parseDSV :: Parser a -> String -> Either String a
parseDSV p s = case parse p "DSV" s of
  Left err -> Left $ show err
  Right x -> Right x

-- | Parse a single record from a string.
fromDSV :: Coalpit a => Options -> String -> Either String a
fromDSV opt str = parseDSV (coalpitParser opt) str

-- | Parse multiple records from a string.
fromDSVList :: Coalpit a => Options -> String -> Either String [a]
fromDSVList opt str =
  parseDSV (coalpitParser opt `sepEndBy` pRecordSep opt) str

-- | Enquote and escape a string, if it contains any characters that
-- need it.
escape :: Options -> String -> String
escape opt str
  | not (null str) &&
    all (\fs -> not (fs `elem` str))
    ('\\'
     : '\"'
     : NE.toList (fieldSeparators opt)
     ++ NE.toList (recordSeparators opt)) = str
  | otherwise = '"' : escaped str ++ "\""
  where
    escaped :: String -> String
    escaped [] = []
    escaped (c:rest)
      | c `elem` "\\\"" = '\\' : c : escaped rest
      | otherwise = c : escaped rest

-- | Build a record ("line") out of individual strings, escaping those
-- if needed.
dsvFromList :: Options -> [String] -> String
dsvFromList opt l = intercalate [NE.head (fieldSeparators opt)]
                    (map (escape opt) l)

-- | Serialize a value.
toDSV :: Coalpit a => Options -> a -> String
toDSV opt x = dsvFromList opt (coalpitPrint opt x)

-- | Serialize multiple values.
toDSVList :: Coalpit a => Options -> [a] -> String
toDSVList opt l =
  concatMap (\x -> toDSV opt x ++ [NE.head (recordSeparators opt)]) l

-- | Compose 'Usage' description.
usage :: Coalpit a => Options -> Proxy a -> Usage
usage opt = coalpitDescription opt []

-- | Compose a usage string.
usageString :: Coalpit a => Options -> Proxy a -> String
usageString opt = usageToString . usage opt

-- | Translate 'Usage' into a string, used by 'usageString'.
usageToString :: Usage -> String
usageToString (UConstructor c) = c
usageToString (URecursive c) = c ++ "..."
usageToString (USelector False s u) = "[" ++ s ++ "] " ++ usageToString u
usageToString (USelector True s u) = s ++ " " ++ usageToString u
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

-- | Parse a field separator.
pFieldSep :: Options -> Parsec String m ()
pFieldSep opt =
  oneOf (NE.toList $ fieldSeparators opt) *> pure ()

-- | Parse a record (line) separator.
pRecordSep :: Options -> Parsec String m ()
pRecordSep opt =
  choice (eof
          : map (\c -> char c *> pure ())
           (NE.toList $ recordSeparators opt))

-- | Parse a token: either a quoted string or a string without
-- unescaped separators. The opposite of 'escape'.
pString :: Options -> Parsec String m String
pString opt =
  (try (quotedString <?> "quoted string"))
  <|> (unquotedString <?> "unquoted string")
  where
    endChars = NE.toList (fieldSeparators opt)
               ++ NE.toList (recordSeparators opt)
    unquotedString = do
      c <- escapedChar endChars
      s <- manyTill (escapedChar endChars)
        (lookAhead $ eof <|> oneOf endChars *> pure ())
      pure (c:s)
    escapedChar ecs = (char '\\' *> oneOf ('\\' : ecs)) <|> anyChar
    quotedString = char '"'
      *> manyTill (escapedChar "\"") (char '"')

-- | Parses a time argument.
pTime :: ParseTime a => Options -> String -> Parser a
pTime opt tf = try $ do
    x <- pString opt
    case readSTime False (timeLocale opt) tf x of
      [(t, "")] -> pure t
      _ -> fail "Failed to parse time"

-- | Read an argument using its 'Read' instance.
pRead :: Read a => Options -> Parser a
pRead opt = do
  x <- pString opt
  case reads x of
    [(n, "")] -> pure n
    _ -> fail $ "Failed to read: " ++ x

-- | Coalpit class: parsing, printing, usage strings.
class Coalpit a where
  coalpitParser :: Options -> Parser a
  default coalpitParser :: (Generic a, GCoalpit (Rep a)) => Options -> Parser a
  coalpitParser opt = to <$> gCoalpitParser opt

  coalpitPrint :: Options -> a -> [String]
  default coalpitPrint :: (Generic a, GCoalpit (Rep a)) => Options -> a -> [String]
  coalpitPrint opt a = gCoalpitPrint opt (from a)

  coalpitDescription :: Options -> [String] -> Proxy a -> Usage
  default coalpitDescription :: (GCoalpit (Rep a))
                    => Options -> [String] -> Proxy a -> Usage
  coalpitDescription opt path Proxy =
    gCoalpitDescription opt path (Proxy :: Proxy (Rep a p))

class GCoalpit a where
  gCoalpitParser :: Options -> Parser (a p)
  gCoalpitPrint :: Options -> a p -> [String]
  gCoalpitDescription :: Options -> [String] -> Proxy (a p) -> Usage


-- Units
instance GCoalpit U1 where
  gCoalpitParser _ = pure U1
  gCoalpitPrint _ U1 = []
  gCoalpitDescription _ _ (Proxy :: Proxy (U1 f)) = UUnit


-- Products
instance (GCoalpit a, GCoalpit b) => GCoalpit (a :*: b) where
  gCoalpitParser opt =
    ((:*:) <$>
     (gCoalpitParser opt <* pFieldSep opt) <*> gCoalpitParser opt)
    <?> "product"
  gCoalpitPrint opt (x :*: y) =
    gCoalpitPrint opt x ++ gCoalpitPrint opt y
  gCoalpitDescription opt path (Proxy :: Proxy ((a :*: b) p)) =
    UProduct (gCoalpitDescription opt path (Proxy :: Proxy (a p)))
    (gCoalpitDescription opt path (Proxy :: Proxy (b p)))


-- Sums
instance
  (GCoalpit a, GCoalpit b) => GCoalpit (a :+: b) where
  gCoalpitParser opt =
    (try (L1 <$> gCoalpitParser opt))
    <|>
    (R1 <$> gCoalpitParser opt)
  gCoalpitPrint opt (L1 x) = gCoalpitPrint opt x
  gCoalpitPrint opt (R1 x) = gCoalpitPrint opt x
  gCoalpitDescription opt path (Proxy :: Proxy ((a :+: b) p)) =
      USum (gCoalpitDescription opt path (Proxy :: Proxy (a p)))
       (gCoalpitDescription opt path (Proxy :: Proxy (b p)))

-- Record Selectors

parseS1 :: (GCoalpit a) => String -> Options -> Parser (S1 selA a p)
parseS1 nameA opt =
  let sName = case (nameA, selNamePolicy opt) of
        ("", _) -> pure ()
        (_, SNDisable) -> pure ()
        (_, SNRequire) -> string (selNameMod opt nameA) *> pFieldSep opt
        (_, _) -> optional
          (try $ (string (selNameMod opt nameA)) *> pFieldSep opt)
  in M1 <$> (sName *> gCoalpitParser opt)

printS1 :: (GCoalpit a, Selector selA) => Options -> S1 selA a p -> [String]
printS1 opt sel@(M1 x) = case (selName sel, selNamePolicy opt) of
                           ("", _) -> gCoalpitPrint opt x
                           (_, SNDisable) -> gCoalpitPrint opt x
                           (_, SNAvoid) -> gCoalpitPrint opt x
                           (name, _) -> selNameMod opt name : gCoalpitPrint opt x

helpS1 :: (GCoalpit a)
       => String -> Options -> [String] -> Proxy (S1 selA a p) -> Usage
helpS1 nameA opt path (Proxy :: Proxy (S1 selA a p)) =
  case (nameA, selNamePolicy opt) of
    ("", _) -> gCoalpitDescription opt path (Proxy :: Proxy (a p))
    (_, SNDisable) -> gCoalpitDescription opt path (Proxy :: Proxy (a p))
    (_, snpol) -> USelector (snpol == SNRequire) (selNameMod opt nameA)
      (gCoalpitDescription opt path (Proxy :: Proxy (a p)))

instance (GCoalpit a, Selector selA) => GCoalpit (S1 selA a) where
  gCoalpitParser = parseS1 (selName (undefined :: S1 selA a p))
  gCoalpitPrint = printS1
  gCoalpitDescription = helpS1 (selName (undefined :: S1 selA a p))

-- Constructors

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
          (gCoalpitDescription opt (nameA : path) (Proxy :: Proxy (a p)))

-- A constructor wrapping just a unit: no field separator is required
-- after such a constructor.
instance {-#OVERLAPPING#-} (Constructor conA) => GCoalpit (C1 conA U1) where
  gCoalpitParser opt =
    ((string
       (conNameMod opt $ conName (undefined :: C1 conA U1 w))
       <?> "constructor name"))
    *> (fmap M1 (gCoalpitParser opt))
  gCoalpitPrint opt (M1 x) = conNameMod opt (conName (undefined :: C1 conA U1 w))
                       : gCoalpitPrint opt x
  gCoalpitDescription opt path (Proxy :: Proxy (C1 conA U1 p)) =
    (handleRecCon (conName (undefined :: C1 conA U1 w)) opt path
     (Proxy :: Proxy (U1 p)))

-- A constructor with non-unit children, with a field separator
-- between constructor name and its children.
instance (GCoalpit a, Constructor conA) => GCoalpit (C1 conA a) where
  gCoalpitParser opt =
    ((string
       (conNameMod opt $ conName (undefined :: C1 conA a w))
       <?> "constructor name"))
    *> (pFieldSep opt)
    *> (fmap M1 (gCoalpitParser opt))
  gCoalpitPrint opt (M1 x) = conNameMod opt (conName (undefined :: C1 conA a w))
                       : gCoalpitPrint opt x
  gCoalpitDescription opt path (Proxy :: Proxy (C1 conA a p)) =
    (handleRecCon (conName (undefined :: C1 conA a w)) opt path
     (Proxy :: Proxy (a p)))

-- Data types
instance (GCoalpit a) => GCoalpit (D1 conA a) where
  gCoalpitParser = fmap M1 . gCoalpitParser
  gCoalpitPrint opt (M1 x) = gCoalpitPrint opt x
  gCoalpitDescription opt path (Proxy :: Proxy (D1 conA a p)) =
    gCoalpitDescription opt path (Proxy :: Proxy (a p))

-- Constraints and such
instance (Coalpit a) => GCoalpit (K1 i a) where
  gCoalpitParser = fmap K1 . coalpitParser
  gCoalpitPrint opt (K1 x) = coalpitPrint opt x
  gCoalpitDescription opt path (Proxy :: Proxy (K1 x a p)) =
    coalpitDescription opt path (Proxy :: Proxy a)


-- Common types

instance Coalpit Int where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "INT"

instance Coalpit Integer where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "INTEGER"

instance Coalpit Word8 where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "WORD8"

instance Coalpit Word16 where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "WORD16"

instance Coalpit Word32 where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "WORD32"

instance Coalpit Word64 where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "WORD64"

instance Coalpit Int8 where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "INT8"

instance Coalpit Int16 where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "INT16"

instance Coalpit Int32 where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "INT32"

instance Coalpit Int64 where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "INT64"

instance Coalpit Natural where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "NATURAL"

instance Coalpit Rational where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "RATIONAL"

instance Coalpit Double where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "DOUBLE"

instance Coalpit Float where
  coalpitParser opt = pRead opt
  coalpitPrint _ i = [show i]
  coalpitDescription _ _ _ = UType "FLOAT"

instance Coalpit Char where
  coalpitParser opt = pRead opt
  coalpitPrint _ c = [show c]
  coalpitDescription _ _ _ = UType "CHAR"


instance {-#OVERLAPPING#-} Coalpit String where
  coalpitParser opt = pString opt
  coalpitPrint _ i = [i]
  coalpitDescription _ _ _ = UType "STRING"

instance Coalpit Scientific where
  coalpitParser opt = try $ do
    x <- pString opt
    case reverse $ readP_to_S scientificP x of
      (n, ""):_ -> pure n
      _ -> fail $ "Failed to read a scientific number: " ++ x
  coalpitPrint opt n = [formatScientific
                  (scientificFormat opt) (scientificDecimals opt) n]
  coalpitDescription _ _ _ = UType "SCIENTIFIC"

instance Coalpit Version where
  coalpitParser opt = try $ do
    x <- pString opt
    case reverse $ readP_to_S parseVersion x of
      (v, ""):_ -> pure v
      _ -> fail $ "Failed to read a version: " ++ x
  coalpitPrint _ v = [showVersion v]
  coalpitDescription _ _ _ = UType "VERSION"


-- | An URI reference (absolute or relative).
instance Coalpit URI where
  coalpitParser opt = try $ do
    x <- pString opt
    maybe (fail $ "Failed to parse URI: " ++ x) pure (parseURIReference x)
  coalpitPrint opt u = [uriToString (uriUserInfo opt) u ""]
  coalpitDescription _ _ _ = UType "URI"


-- | Uses 'dateTimeFormat'.
instance Coalpit UTCTime where
  coalpitParser opt = pTime opt (dateTimeFormat opt)
  coalpitPrint opt t = [formatTime (timeLocale opt) (dateTimeFormat opt) t]
  coalpitDescription _ _ _ = UType "UTC_TIME"

-- | Uses 'dateTimeFormat'.
instance Coalpit ZonedTime where
  coalpitParser opt = pTime opt (dateTimeFormat opt)
  coalpitPrint opt t = [formatTime (timeLocale opt) (dateTimeFormat opt) t]
  coalpitDescription _ _ _ = UType "ZONED_TIME"

-- | Uses 'dateTimeFormat'.
instance Coalpit LocalTime where
  coalpitParser opt = pTime opt (dateTimeFormat opt)
  coalpitPrint opt t = [formatTime (timeLocale opt) (dateTimeFormat opt) t]
  coalpitDescription _ _ _ = UType "LOCAL_TIME"

-- | Uses 'dateTimeFormat'.
instance Coalpit UniversalTime where
  coalpitParser opt = pTime opt (dateTimeFormat opt)
  coalpitPrint opt t = [formatTime (timeLocale opt) (dateTimeFormat opt) t]
  coalpitDescription _ _ _ = UType "UNIVERSAL_TIME"

-- | Uses 'timeFormat'.
instance Coalpit TimeOfDay where
  coalpitParser opt = pTime opt (timeFormat opt)
  coalpitPrint opt t = [formatTime (timeLocale opt) (timeFormat opt) t]
  coalpitDescription _ _ _ = UType "TIME_OF_DAY"

-- | Uses 'dateFormat'.
instance Coalpit Day where
  coalpitParser opt = pTime opt (dateFormat opt)
  coalpitPrint opt t = [formatTime (timeLocale opt) (dateFormat opt) t]
  coalpitDescription _ _ _ = UType "DAY"

-- | Converts to/from 'Scientific'.
instance Coalpit NominalDiffTime where
  coalpitParser opt = fromRational . toRational
                  <$> (coalpitParser opt :: Parser Scientific)
  coalpitPrint opt = coalpitPrint opt .
    (fromRational . toRational :: NominalDiffTime -> Scientific)
  coalpitDescription _ _ _ = UType "NOMINAL_DIFF_TIME"

-- | Converts to/from 'Scientific'.
instance Coalpit DiffTime where
  coalpitParser opt = fromRational . toRational
                  <$> (coalpitParser opt :: Parser Scientific)
  coalpitPrint opt = coalpitPrint opt .
    (fromRational . toRational :: DiffTime -> Scientific)
  coalpitDescription _ _ _ = UType "DIFF_TIME"

instance Coalpit ()
instance Coalpit Bool
instance Coalpit Ordering
instance Coalpit ExitCode
instance Coalpit a => Coalpit (Complex a)
instance Coalpit a => Coalpit (Maybe a)
instance Coalpit a => Coalpit [a]
instance Coalpit a => Coalpit (NonEmpty a)
instance (Coalpit a, Coalpit b) => Coalpit (Either a b)
instance (Coalpit a, Coalpit b) => Coalpit (a, b)
instance (Coalpit a, Coalpit b, Coalpit c) => Coalpit (a, b, c)
instance (Coalpit a, Coalpit b, Coalpit c, Coalpit d) => Coalpit (a, b, c, d)
