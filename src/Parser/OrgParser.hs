{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Parser.OrgParser where

import Data.Char (isDigit, isLower)
import Data.List (find)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Time
import GHC.Base hiding (foldr)
import Model.OrgMode
import Model.Tree
import Parser.Parser
import Parser.StandardParsers
import TextUtils

-------------------------------- ORG MODE -------------------------------------

taskLevelParser :: Parser Int
taskLevelParser = failOnConditionParser parser (<= 0) errorMsg
 where
  parser = fmap T.length (takeWhileParser (== '*'))
  errorMsg = "Task level must be specified with at least one '*'"

-- TODO: make it read only uppercase letters
todoKeyWordParser :: Parser T.Text
todoKeyWordParser = wordParser

priorityParser :: Parser Int
priorityParser = stringParser "[#" *> letterToPriorityParser <* charParser ']'
 where
  letterToPriorityParser = failOnConditionParser (fmap (\c -> ord c - ord 'A') singleCharParser) (\i -> i < 0 || i > ord 'Z' - ord 'A') "Got invalid priority letter"

isTagChar :: Char -> Bool
isTagChar c = isLower c || isDigit c

-------------------------------------------------------------------------------

titleAndTagsParser :: Parser (T.Text, [T.Text])
titleAndTagsParser = fmap splitToTitleAndTags tillTheEndOfStringParser

splitToTitleAndTags :: T.Text -> (T.Text, [T.Text])
splitToTitleAndTags input = (T.strip actualTitle, actualTags)
 where
  parts = split ':' input
  (titleParts, tagParts) = break isTag parts
  title = T.concat titleParts
  tags = filter (not . T.null) $ fmap stripLeadingColumn tagParts

  (actualTitle, actualTags) = case reverse tagParts of
    [] -> (title, [])
    x : _ -> if x == ":" then (title, tags) else (input, [])

  isTag :: T.Text -> Bool
  isTag str = case T.uncons str of
    Nothing -> False
    Just (x, xs) -> x == ':' && T.all isTagChar xs

  stripLeadingColumn :: T.Text -> T.Text
  stripLeadingColumn str = case T.uncons str of
    Nothing -> ""
    Just (x, xs) -> if x == ':' then xs else str

-------------------------------------------------------------------------------

toOrgDateTime :: T.Text -> Maybe UTCTime
toOrgDateTime str = parseTimeM True defaultTimeLocale "%Y-%m-%d %a %H:%M" (T.unpack str)

toOrgDate :: T.Text -> Maybe Day
toOrgDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d %a" (T.unpack str)

parseDateOrDateTime :: T.Text -> Maybe OrgTime
parseDateOrDateTime input =
  case (toOrgDateTime input, toOrgDate input) of
    (Just dateTime, _) -> Just $ Right dateTime
    (Nothing, Just date) -> Just $ Left date
    _ -> Nothing

dateTimeParser :: Char -> Parser (Maybe OrgTime)
dateTimeParser delimiterRight = fmap parseDateOrDateTime (splitParser (T.span (/= delimiterRight)))

-- TODO: return error instead of nothing
timePropertyParser :: T.Text -> (Char, Char) -> Parser (Maybe OrgTime)
timePropertyParser field (delimiterLeft, delimiterRight) =
  stringParser field
    *> charParser ':'
    *> skipBlanksParser
    *> charParser delimiterLeft
    *> dateTimeParser delimiterRight
    <* charParser delimiterRight

-- TODO: return error instead of nothing, so the last pure Nothing won't be
-- necessary
scheduledOrDeadLineParser :: Parser (Maybe (T.Text, OrgTime))
scheduledOrDeadLineParser =
  makeP "SCHEDULED" angleDelim
    <|> makeP "DEADLINE" angleDelim
    <|> makeP "CLOSED" bracketDelim
    <|> pure Nothing
 where
  angleDelim = ('<', '>')
  bracketDelim = ('[', ']')
  makeP field delim = fmap (fmap (field,)) (timePropertyParser field delim)

propertyParser :: Parser (T.Text, T.Text)
propertyParser =
  charParser ':'
    *> ( (\a _ b -> (a, b))
          <$> wordParser
          <*> stringParser ": "
          <*> tillTheEndOfStringParser
          <* charParser '\n'
       )

propertiesParser :: Parser [(T.Text, T.Text)]
propertiesParser =
  stringParser ":PROPERTIES:"
    *> skipBlanksParser
    *> many propertyParser
    <* skipBlanksParser
    <* stringParser ":END:"

descriptionParser :: Parser T.Text
descriptionParser = takeUntilDelimParser "\n*"

findProp :: T.Text -> [(T.Text, a)] -> Maybe a
findProp name l = snd <$> find (\(n, _) -> n == name) l

removeDelimiters :: T.Text -> Maybe T.Text
removeDelimiters t
  | T.length t >= 2 = Just $ T.init $ T.tail t
  | otherwise = Nothing

properTaskParser :: Parser Task
properTaskParser =
  ( \level todoKeyword priority (title, tags) timeProp1 timeProp2 timeProp3 properties description ->
      let
        propsList = catMaybes [timeProp1, timeProp2, timeProp3]
       in
        Task
          level
          todoKeyword
          priority
          title
          tags
          (findProp "SCHEDULED" propsList)
          (findProp "DEADLINE" propsList)
          (findProp "CLOSED" propsList)
          (findProp "CREATED" properties >>= removeDelimiters >>= parseDateOrDateTime)
          (filter (\(k, _) -> k /= "CREATED") properties)
          description
  )
    <$> (skipBlanksParser *> taskLevelParser)
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> (skipBlanksParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksParser *> propertiesParser)
    <*> descriptionParser

brokenDescriptionTaskParser :: Parser Task
brokenDescriptionTaskParser =
  ( \level todoKeyword priority (title, tags) description timeProp1 timeProp2 timeProp3 properties ->
      let
        propsList = catMaybes [timeProp1, timeProp2, timeProp3]
       in
        Task
          level
          todoKeyword
          priority
          title
          tags
          (findProp "SCHEDULED" propsList)
          (findProp "DEADLINE" propsList)
          (findProp "CLOSED" propsList)
          (findProp "CREATED" properties >>= parseDateOrDateTime)
          (("BROKEN_DESCRIPTION", "TRUE") : properties)
          description
  )
    <$> (skipBlanksParser *> taskLevelParser)
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> (skipBlanksParser *> descriptionParser)
    <*> (skipBlanksParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksParser *> propertiesParser)

brokenPropertiesTaskParser :: Parser Task
brokenPropertiesTaskParser =
  ( \level todoKeyword priority (title, tags) timeProp1 timeProp2 timeProp3 description ->
      let
        propsList = catMaybes [timeProp1, timeProp2, timeProp3]
       in
        Task
          level
          todoKeyword
          priority
          title
          tags
          (findProp "SCHEDULED" propsList)
          (findProp "DEADLINE" propsList)
          (findProp "CLOSED" propsList)
          Nothing
          [("BROKEN_PROPERTIES", "TRUE")]
          description
  )
    <$> (skipBlanksParser *> taskLevelParser)
    <*> (skipBlanksExceptNewLinesParser *> todoKeyWordParser)
    <*> (skipBlanksExceptNewLinesParser *> maybeParser priorityParser)
    <*> (skipBlanksExceptNewLinesParser *> titleAndTagsParser)
    <*> (skipBlanksParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksExceptNewLinesParser *> scheduledOrDeadLineParser)
    <*> (skipBlanksParser *> descriptionParser)

anyTaskparser :: Parser Task
anyTaskparser =
  properTaskParser
    <|> brokenDescriptionTaskParser
    <|> brokenPropertiesTaskParser

allTasksParser :: Parser (Forest Task)
allTasksParser =
  many anyTaskparser
    >>= ( \case
            Left err -> failingParser $ "Forest Construction Failed: " ++ err
            Right forest -> succeedingParser forest
        )
      . (`makeForest` (\t -> level t - 1))

orgFileParser :: Parser TaskFile
orgFileParser = fmap (uncurry TaskFile) parser
 where
  fileTitleParser = maybeParser $ stringParser "#+TITLE: " *> tillTheEndOfStringParser <* skipBlanksParser
  parser = (,) <$> fileTitleParser <*> allTasksParser
