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
--  * Keys cannot contain @:@, @=@, @[@, or @]@.
--
--  * Comments are not supported at this time.
--
-- An example configuration file:
--
-- @
-- [SERVER]
-- port=6667
-- hostname=localhost
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
   parseIniFile
  ,parseIni
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
parseIniFile :: FilePath -> IO (Either String Ini)
parseIniFile = fmap parseIni . T.readFile

-- | Parse an INI config.
parseIni :: Text -> Either String Ini
parseIni = parseOnly iniParser

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
          name <> ": " <> value

-- | Parser for an INI.
iniParser :: Parser Ini
iniParser = fmap Ini (fmap M.fromList (many1 sectionParser))

-- | A section. Format: @[foo]@. Conventionally, @[FOO]@.
sectionParser :: Parser (Text,HashMap Text Text)
sectionParser =
  do char '['
     name <- takeWhile (\c -> c /=']' && c /= '[')
     char ']'
     skipEndOfLine
     values <- many1 keyValueParser
     return (name,M.fromList values)

-- | A key-value pair. Either @foo: bar@ or @foo=bar@.
keyValueParser :: Parser (Text,Text)
keyValueParser =
  do key <- takeWhile1 (\c -> not (isDelim c || c == '[' || c == ']'))
     delim <- satisfy isDelim
     value <- fmap (clean delim) (takeWhile (not . isEndOfLine))
     skipEndOfLine
     return (key,value)
  where clean ':' = T.drop 1
        clean _   = id

-- | Is the given character a delimiter?
isDelim :: Char -> Bool
isDelim x = x == '=' || x == ':'

-- | Skip end of line and whitespace beyond.
skipEndOfLine :: Parser ()
skipEndOfLine = skipWhile (\c -> isEndOfLine c || isSpace c)
