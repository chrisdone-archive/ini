{-# LANGUAGE OverloadedStrings #-}

-- | Clean configuration files in the INI format.
--
-- Format rules and recommendations:
--
--  * The @: @ syntax is space-sensitive.
--
--  * Keys are case-sensitive.
--
--  * Lower-case is recommended.
--
--  * Values can be empty.
--
--  * Keys cannot key separators, section delimiters, or comment markers.
--
--  * Comments must start at the beginning of the line and start with @;@ or @#@.
--
-- An example configuration file:
--
-- @
-- # Some comment.
-- [SERVER]
-- port=6667
-- hostname=localhost
-- ; another comment here
-- [AUTH]
-- user: hello
-- pass: world
-- salt:
-- @
--
-- Parsing example:
--
-- >>> parseIni "[SERVER]\nport: 6667\nhostname: localhost"
-- Right (Ini {unIni = fromList [("SERVER",fromList [("hostname","localhost"),("port","6667")])]})
--

module Data.Ini
  (-- * Reading
   readIniFile
  ,parseIni
  ,lookupValue
  ,readValue
  ,parseValue
   -- * Writing
  ,writeIniFile
  ,printIni
   -- * Types
  ,Ini(..)
   -- * Parsers
  ,iniParser
  ,sectionParser
  ,keyValueParser
  )
  where

import           Control.Monad
import           Control.Applicative ((<*))
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Prelude hiding (takeWhile)

-- | An INI configuration.
newtype Ini = Ini { unIni :: HashMap Text (HashMap Text Text) }
  deriving (Show)

-- | Parse an INI file.
readIniFile :: FilePath -> IO (Either String Ini)
readIniFile = fmap parseIni . T.readFile

-- | Parse an INI config.
parseIni :: Text -> Either String Ini
parseIni = parseOnly iniParser

-- | Lookup values in the config.
lookupValue :: Text -> Text -> Ini -> Either String Text
lookupValue name key (Ini ini) =
  case M.lookup name ini of
    Nothing -> Left ("Couldn't find section: " ++ T.unpack name)
    Just section ->
      case M.lookup key section of
        Nothing -> Left ("Couldn't find key: " ++ T.unpack key)
        Just value -> return value

-- | Read a value using a reader from "Data.Text.Read".
readValue :: Text -> Text -> (Text -> Either String (a, Text))
          -> Ini
          -> Either String a
readValue section key f ini =
  lookupValue section key ini >>= f >>= return . fst

-- | Parse a value using a reader from "Data.Attoparsec.Text".
parseValue :: Text -> Text -> Parser a
           -> Ini
           -> Either String a
parseValue section key f ini =
  lookupValue section key ini >>= parseOnly (f <* (skipSpace >> endOfInput))

-- | Print the INI config to a file.
writeIniFile :: FilePath -> Ini -> IO ()
writeIniFile fp = T.writeFile fp . printIni

-- | Print an INI config.
printIni :: Ini -> Text
printIni (Ini sections) =
  T.concat (map buildSection (M.toList sections))
  where buildSection (name,pairs) =
          "[" <> name <> "]\n" <>
          T.concat (map buildPair (M.toList pairs))
        buildPair (name,value) =
          name <> ": " <> value <> "\n"

-- | Parser for an INI.
iniParser :: Parser Ini
iniParser = fmap Ini (fmap M.fromList (many1 sectionParser))

-- | A section. Format: @[foo]@. Conventionally, @[FOO]@.
sectionParser :: Parser (Text,HashMap Text Text)
sectionParser =
  do skipComments
     _ <- char '['
     name <- takeWhile (\c -> c /=']' && c /= '[')
     _ <- char ']'
     skipEndOfLine
     values <- many1 keyValueParser
     return (T.strip name, M.fromList values)

-- | A key-value pair. Either @foo: bar@ or @foo=bar@.
keyValueParser :: Parser (Text,Text)
keyValueParser =
  do skipComments
     key <- takeWhile1 (\c -> not (isDelim c || c == '[' || c == ']'))
     delim <- satisfy isDelim
     value <- fmap (clean delim) (takeWhile (not . isEndOfLine))
     skipEndOfLine
     return (T.strip key, T.strip value)
  where clean ':' = T.drop 1
        clean _   = id

-- | Is the given character a delimiter?
isDelim :: Char -> Bool
isDelim x = x == '=' || x == ':'

-- | Skip end of line and whitespace beyond.
skipEndOfLine :: Parser ()
skipEndOfLine = skipWhile (\c -> isEndOfLine c)

-- | Skip comments starting at the beginning of the line.
skipComments :: Parser ()
skipComments =
  skipMany (do _ <- satisfy (\c -> c == ';' || c == '#')
               skipWhile (not . isEndOfLine)
               skipEndOfLine)
