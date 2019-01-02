{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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
  ,lookupArray
  ,readValue
  ,readArray
  ,parseValue
  ,sections
  ,keys
   -- * Writing
  ,printIni
  ,writeIniFile
   -- * Advanced writing
  ,KeySeparator(..)
  ,WriteIniSettings(..)
  ,defaultWriteIniSettings
  ,printIniWith
  ,writeIniFileWith
   -- * Types
  ,Ini(..)
  ,unIni
   -- * Parsers
  ,iniParser
  ,sectionParser
  ,keyValueParser
  )
  where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Combinator
import           Data.Attoparsec.Text
import           Data.Char
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as M
import Data.Maybe
import           Data.Monoid (Monoid)
import           Data.Semigroup
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Prelude                    hiding (takeWhile)

-- | An INI configuration.
data Ini =
  Ini
    { iniSections :: HashMap Text [(Text, Text)]
    , iniGlobals  :: [(Text, Text)]
    }
  deriving (Show, Eq)

instance Semigroup Ini where
  (<>) = mappend

instance Monoid Ini where
  mempty = Ini {iniGlobals = mempty, iniSections = mempty}
  mappend x y =
    Ini {iniGlobals = mempty, iniSections = iniSections x <> iniSections y}

{-# DEPRECATED #-}
unIni :: Ini -> HashMap Text (HashMap Text Text)
unIni = fmap M.fromList . iniSections

-- | Parse an INI file.
readIniFile :: FilePath -> IO (Either String Ini)
readIniFile = fmap parseIni . T.readFile

-- | Parse an INI config.
parseIni :: Text -> Either String Ini
parseIni = parseOnly iniParser

-- | Lookup one value in the config.
--
-- Example:
--
-- >>> parseIni "[SERVER]\nport: 6667\nhostname: localhost" >>= lookupValue "SERVER" "hostname"
-- Right "localhost"
lookupValue :: Text -- ^ Section name
            -> Text -- ^ Key
            -> Ini -> Either String Text
lookupValue name key (Ini {iniSections=secs}) =
  case M.lookup name secs of
    Nothing -> Left ("Couldn't find section: " ++ T.unpack name)
    Just section ->
      case lookup key section of
        Nothing -> Left ("Couldn't find key: " ++ T.unpack key)
        Just value -> return value

-- | Lookup one value in the config.
--
-- Example:
--
-- >>> parseIni "[SERVER]\nport: 6667\nhostname: localhost" >>= lookupValue "SERVER" "hostname"
-- Right "localhost"
lookupArray :: Text -- ^ Section name
            -> Text -- ^ Key
            -> Ini -> Either String [Text]
lookupArray name key (Ini {iniSections = secs}) =
  case M.lookup name secs of
    Nothing -> Left ("Couldn't find section: " ++ T.unpack name)
    Just section ->
      case mapMaybe
             (\(k, v) ->
                if k == key
                  then Just v
                  else Nothing)
             section of
        [] -> Left ("Couldn't find key: " ++ T.unpack key)
        values -> return values

-- | Get the sections in the config.
--
-- Example:
--
-- >>> sections <$> parseIni "[SERVER]\nport: 6667\nhostname: localhost"
-- Right ["SERVER"]
sections :: Ini -> [Text]
sections = M.keys . iniSections

-- | Get the keys in a section.
--
-- Example:
--
-- >>> parseIni "[SERVER]\nport: 6667\nhostname: localhost" >>= keys "SERVER"
-- Right ["hostname","port"]
keys :: Text -- ^ Section name
     -> Ini -> Either String [Text]
keys name i =
  case M.lookup name (iniSections i) of
    Nothing -> Left ("Couldn't find section: " ++ T.unpack name)
    Just section -> Right (map fst section)

-- | Read a value using a reader from "Data.Text.Read".
readValue :: Text -- ^ Section name
          -> Text -- ^ Key
          -> (Text -> Either String (a, Text))
          -> Ini
          -> Either String a
readValue section key f ini =
  lookupValue section key ini >>= f >>= return . fst

-- | Read an array of values using a reader from "Data.Text.Read".
readArray :: Text -- ^ Section name
          -> Text -- ^ Key
          -> (Text -> Either String (a, Text))
          -> Ini
          -> Either String [a]
readArray section key f ini =
  fmap (map fst) (lookupArray section key ini >>= mapM f)

-- | Parse a value using a reader from "Data.Attoparsec.Text".
parseValue :: Text -- ^ Section name
           -> Text -- ^ Key
           -> Parser a
           -> Ini
           -> Either String a
parseValue section key f ini =
  lookupValue section key ini >>= parseOnly (f <* (skipSpace >> endOfInput))

-- | Print the INI config to a file.
writeIniFile :: FilePath -> Ini -> IO ()
writeIniFile = writeIniFileWith defaultWriteIniSettings

-- | Print an INI config.
printIni :: Ini -> Text
printIni = printIniWith defaultWriteIniSettings

-- | Either @:@ or @=@.
data KeySeparator
  = ColonKeySeparator
  | EqualsKeySeparator
  deriving (Eq, Show)

-- | Settings determining how an INI file is written.
data WriteIniSettings = WriteIniSettings
  { writeIniKeySeparator :: KeySeparator
  } deriving (Show)

-- | The default settings for writing INI files.
defaultWriteIniSettings :: WriteIniSettings
defaultWriteIniSettings = WriteIniSettings
  { writeIniKeySeparator = ColonKeySeparator
  }

-- | Print the INI config to a file.
writeIniFileWith :: WriteIniSettings -> FilePath -> Ini -> IO ()
writeIniFileWith wis fp = T.writeFile fp . printIniWith wis

-- | Print an INI config.
printIniWith :: WriteIniSettings -> Ini -> Text
printIniWith wis i =
  T.concat (map buildSection (M.toList (iniSections i)))
  where buildSection (name,pairs) =
          "[" <> name <> "]\n" <>
          T.concat (map buildPair pairs)
        buildPair (name,value) =
          name <> separator <> value <> "\n"
        separator = case writeIniKeySeparator wis of
          ColonKeySeparator  -> ": "
          EqualsKeySeparator -> "="

-- | Parser for an INI.
iniParser :: Parser Ini
iniParser =
  (\kv secs -> Ini {iniSections = M.fromList secs, iniGlobals = kv}) <$>
  many keyValueParser <*>
  many sectionParser

-- | A section. Format: @[foo]@. Conventionally, @[FOO]@.
sectionParser :: Parser (Text,[(Text, Text)])
sectionParser =
  do skipEndOfLine
     skipComments
     skipEndOfLine
     _ <- char '['
     name <- takeWhile (\c -> c /=']' && c /= '[')
     _ <- char ']'
     skipEndOfLine
     values <- many keyValueParser
     return (T.strip name, values)

-- | A key-value pair. Either @foo: bar@ or @foo=bar@.
keyValueParser :: Parser (Text,Text)
keyValueParser =
  do skipEndOfLine
     skipComments
     skipEndOfLine
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
skipEndOfLine = skipWhile isSpace

-- | Skip comments starting at the beginning of the line.
skipComments :: Parser ()
skipComments =
  skipMany (do _ <- satisfy (\c -> c == ';' || c == '#')
               skipWhile (not . isEndOfLine)
               skipEndOfLine)
