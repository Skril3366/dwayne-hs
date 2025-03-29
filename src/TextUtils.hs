{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module TextUtils where

import Data.Char (isSpace)
import Data.Kind
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (ParseTime, defaultTimeLocale)
import Data.Time.Format (parseTimeM)

readFileExample :: FilePath -> IO T.Text
readFileExample = TIO.readFile

writeFileExample :: FilePath -> T.Text -> IO ()
writeFileExample = TIO.writeFile

printExample :: T.Text -> IO ()
printExample = TIO.putStrLn

splitBy :: Char -> T.Text -> [T.Text]
splitBy _ "" = []
splitBy delimiterChar inputString = T.foldr f [T.pack ""] inputString
 where
  f :: Char -> [T.Text] -> [T.Text]
  f _ [] = []
  f currentChar allStrings@(partialString : handledStrings)
    | currentChar == delimiterChar = "" : allStrings
    | otherwise = T.cons currentChar partialString : handledStrings

isWhitespaceExceptNewline :: Char -> Bool
isWhitespaceExceptNewline c = isSpace c && c /= '\n' && c /= '\r'

splitByFirstDelimiter :: T.Text -> T.Text -> (T.Text, T.Text)
splitByFirstDelimiter _ "" = ("", "")
splitByFirstDelimiter delim input
  | delim `T.isPrefixOf` input = ("", input)
  | otherwise =
      let (prefix, rest) = splitByFirstDelimiter delim (T.tail input)
       in (T.cons (T.head input) prefix, rest)

split :: Char -> T.Text -> [T.Text]
split _ "" = []
split delim str =
  let (prefix, suffix) = T.break (== delim) str
   in prefix : case T.uncons suffix of
        Nothing -> []
        Just (_, rest) -> case split delim rest of
          [] -> [":"]
          x : xs -> T.cons delim x : xs

removeLeadingSpaces :: T.Text -> T.Text
removeLeadingSpaces = T.dropWhile isSpace

parseTimeWith :: forall (m :: Type -> Type) t. (MonadFail m, ParseTime t) => String -> T.Text -> m t
parseTimeWith format str = parseTimeM True defaultTimeLocale format (T.unpack str)
