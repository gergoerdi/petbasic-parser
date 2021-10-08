module Text

import Data.String
import Data.List
import System.File.Buffer
import Data.Buffer


readable : Bits8 -> Char
readable b = case the Char (cast b) of
  '\r' => '\n'
  '&' => ','
  '\193' => 'Á'
  '\197' => 'É'
  '\207' => 'Ó'
  '^' => 'Ö'
  '*' => 'Ő'
  '\213' => 'Ú'
  '\169' => 'Ü'
  '\\' => 'Ű'
  '\201' => 'Í'
  c => c

export
sanitizeLine : List Bits8 -> String
sanitizeLine =
  pack .
  filter (not . isControl) .
  map readable .
  dropWhile (<= 0x30)

export
loadText : HasIO io => String -> io (List String)
loadText textFile = do
  Right buf <- createBufferFromFile textFile
    | Left _ => assert_total $ idris_crash $ unwords ["createBufferFromFile:", textFile]
  bs <- map cast . drop 2 <$> bufferData buf
  pure $ filter (not . null) $ lines $ pack . map readable $ bs
