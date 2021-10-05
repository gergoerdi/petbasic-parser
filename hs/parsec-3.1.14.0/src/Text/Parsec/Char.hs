{-# LANGUAGE CPP, FlexibleContexts, Safe #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Char
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Commonly used character parsers.
--
-----------------------------------------------------------------------------

module Text.Parsec.Char where

import Data.Char
import Text.Parsec.Pos
import Text.Parsec.Prim
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative ((*>))
#endif

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- >   vowel  = oneOf "aeiou"

oneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
oneOf cs            = satisfy (\c -> elem c cs)

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- >  consonant = noneOf "aeiou"

noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
noneOf cs           = satisfy (\c -> not (elem c cs))

-- | Skips /zero/ or more white space characters. See also 'skipMany'.

spaces :: (Stream s m Char) => ParsecT s u m ()
spaces              = skipMany space        <?> "white space"

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character.

space :: (Stream s m Char) => ParsecT s u m Char
space               = satisfy isSpace       <?> "space"

-- | Parses a newline character (\'\\n\'). Returns a newline character.

newline :: (Stream s m Char) => ParsecT s u m Char
newline             = char '\n'             <?> "lf new-line"

-- | Parses a carriage return character (\'\\r\') followed by a newline character (\'\\n\').
-- Returns a newline character.

crlf :: (Stream s m Char) => ParsecT s u m Char
crlf                = char '\r' *> char '\n' <?> "crlf new-line"

-- | Parses a CRLF (see 'crlf') or LF (see 'newline') end-of-line.
-- Returns a newline character (\'\\n\').
--
-- > endOfLine = newline <|> crlf
--

endOfLine :: (Stream s m Char) => ParsecT s u m Char
endOfLine           = newline <|> crlf       <?> "new-line"

-- | Parses a tab character (\'\\t\'). Returns a tab character.

tab :: (Stream s m Char) => ParsecT s u m Char
tab                 = char '\t'             <?> "tab"

-- | Parses an upper case letter (according to 'isUpper').
-- Returns the parsed character.

upper :: (Stream s m Char) => ParsecT s u m Char
upper               = satisfy isUpper       <?> "uppercase letter"

-- | Parses a lower case character (according to 'isLower').
-- Returns the parsed character.

lower :: (Stream s m Char) => ParsecT s u m Char
lower               = satisfy isLower       <?> "lowercase letter"

-- | Parses a alphabetic or numeric Unicode characters
-- according to 'isAlphaNum'. Returns the parsed character.
--
-- Note that numeric digits outside the ASCII range (such as arabic-indic digits like e.g. \"٤\" or @U+0664@),
-- as well as numeric characters which aren't digits, are parsed by this function
-- but not by 'digit'.

alphaNum :: (Stream s m Char => ParsecT s u m Char)
alphaNum            = satisfy isAlphaNum    <?> "letter or digit"

-- | Parses an alphabetic Unicode characters (lower-case, upper-case and title-case letters,
-- plus letters of caseless scripts and modifiers letters according to 'isAlpha').
-- Returns the parsed character.

letter :: (Stream s m Char) => ParsecT s u m Char
letter              = satisfy isAlpha       <?> "letter"

-- | Parses an ASCII digit. Returns the parsed character.

digit :: (Stream s m Char) => ParsecT s u m Char
digit               = satisfy isDigit       <?> "digit"

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\'). Returns the parsed character.

hexDigit :: (Stream s m Char) => ParsecT s u m Char
hexDigit            = satisfy isHexDigit    <?> "hexadecimal digit"

-- | Parses an octal digit (a character between \'0\' and \'7\'). Returns
-- the parsed character.

octDigit :: (Stream s m Char) => ParsecT s u m Char
octDigit            = satisfy isOctDigit    <?> "octal digit"

-- | @char c@ parses a single character @c@. Returns the parsed
-- character (i.e. @c@).
--
-- >  semiColon  = char ';'

char :: (Stream s m Char) => Char -> ParsecT s u m Char
char c              = satisfy (==c)  <?> show [c]

-- | This parser succeeds for any character. Returns the parsed character.

anyChar :: (Stream s m Char) => ParsecT s u m Char
anyChar             = satisfy (const True)

-- | The parser @satisfy f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.

-- >  digit     = satisfy isDigit
-- >  oneOf cs  = satisfy (\c -> c `elem` cs)

satisfy :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
satisfy f           = tokenPrim (\c -> show [c])
                                (\pos c _cs -> updatePosChar pos c)
                                (\c -> if f c then Just c else Nothing)

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- >  divOrMod    =   string "div"
-- >              <|> string "mod"

string :: (Stream s m Char) => String -> ParsecT s u m String
string s            = tokens show updatePosString s
