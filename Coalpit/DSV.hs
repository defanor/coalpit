{- |
Module      :  Coalpit.DSV
Description :  DSV printing and parsing
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (uses GHC extensions)

This module provides functions for DSV printing and parsing.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Coalpit.DSV (showDSV, readDSV) where

import Data.List
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

import Coalpit.Core


composeDSVLine :: Char -> [String] -> String
composeDSVLine fs = intercalate [fs] . map escapeVal
  where
    escapeVal :: String -> String
    -- not great, but will do for now
    escapeVal s = let inner = show s
                  in if fs `elem` inner
                     then inner
                     else init $ tail inner

pStr :: Char -> Parsec Void String String
pStr fs = do
  s <- try (between (char '"') (char '"')
            (concat <$> many (string "\\\\"
                              <|> string "\\\""
                              <|> pure <$> notChar '"')))
    <|> many (notChar fs)
  case reads (concat ["\"", s, "\""]) of
    [(str, "")] -> pure str
    other -> fail $ "Failed to read a string: " ++ show other ++ "(" ++ s ++ ")"

pDSVLine :: Char -> Parsec Void String [String]
pDSVLine fs = pStr fs `sepBy` char fs

parseDSVLine :: Char -> String -> Either String [String]
parseDSVLine fs l = case parse (pDSVLine fs) "line" l of
  Left err -> Left $ parseErrorPretty err
  Right x -> Right x

-- | Shows values in DSV format.
showDSV :: Coalpit a => Options -> a -> String
showDSV opt = composeDSVLine (fieldSeparator opt) . toArgs opt

-- | Reads values from DSV format.
readDSV :: Coalpit a => Options -> String -> Either String a
readDSV opt = (>>= fromArgs opt) . parseDSVLine (fieldSeparator opt)
