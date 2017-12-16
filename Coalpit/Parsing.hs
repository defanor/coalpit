{- |
Module      :  Coalpit.Parsing
Description :  Argument parsing facilities
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

This module provides functions useful for argument parsing.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Coalpit.Parsing ( Parser
                       , CLArg(..)
                       , pS
                       , readArg
                       , pTime
                       ) where

import Text.Megaparsec
import Data.Proxy (Proxy(..))
import Data.Time.Format (TimeLocale, ParseTime, readSTime)
import Data.Void (Void)
import qualified Data.List.NonEmpty as NE
import Data.List (foldl')
import Data.Semigroup ((<>))

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

-- | Reads an argument using its 'Read' instance.
readArg :: Read a => Parser a
readArg = do
  x <- token (Right . unArg) Nothing
  case reads x of
    [(n, "")] -> pure n
    _ -> fail $ "Failed to read: " ++ x

-- | Parses a time argument.
pTime :: ParseTime a
      => TimeLocale
      -- ^ Options, to read 'timeLocale' from.
      -> String
      -- ^ Time format to use.
      -> Parser a
pTime tl tf = try $ do
    x <- token (Right . unArg) Nothing
    case readSTime False tl tf x of
      [(t, "")] -> pure t
      _ -> fail "Failed to parse time"
