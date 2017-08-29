module IO (
  readFileWithEncoding,
  writeFileWithEncoding,
  latin1, char8,
  utf8, utf8_bom,
  utf16, utf16be, utf16le,
  utf32, utf32le, utf32be,
  localeEncoding, mkTextEncoding) where

import System.IO

readFileWithEncoding :: FilePath -> TextEncoding -> IO String
readFileWithEncoding file enc = do
  handle <- openFile file ReadMode
  hSetEncoding handle enc
  contents <- hGetContents handle
  hClose handle
  return contents

writeFileWithEncoding :: FilePath -> TextEncoding -> String -> IO ()
writeFileWithEncoding file enc str = do
  handle <- openFile file ReadMode
  hSetEncoding handle enc
  hPutStr handle str
  hClose handle
