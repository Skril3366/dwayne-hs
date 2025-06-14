{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Render.OrgRender where

import Brick
import Control.Lens (view)
import Data.Char (ord)
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time (defaultTimeLocale)
import Data.Time.Format (formatTime)
import GHC.Char (chr)
import Graphics.Vty.Attributes (defAttr)
import Model.Injection
import Model.OrgMode
import Render.Render
import Tui.ColorScheme (ColorScheme, levelAttr, todoKeywordAttr, priorityAttr, tagAttr, timeFieldAttr, propertyAttr, descriptionAttr)

-- TODO: factor out common functions from it and Writer
instance RenderTask Task b where
  renderCompact task = titleLine
   where
    titleLine =
      txt $
        T.unwords $
          catMaybes
            [ Just $ T.replicate (view level task) "*"
            , Just $ view todoKeyword task -- TODO: colorcode them
            , view priority task >>= renderPriority
            , Just (view title task)
            , renderTags (view tags task)
            ]

    renderPriority p
      | p >= 0 = Just $ T.concat ["[#", T.singleton (chr $ ord 'A' + p), "]"]
      | otherwise = Nothing

    renderTags [] = Nothing
    renderTags ts = Just $ T.concat [":", T.intercalate ":" ts, ":"]

  renderCompactWithColors scheme task =
    hBox $ catMaybes
      [ Just $ withAttr (levelAttr (view level task)) $ txt $ T.replicate (view level task) "*"
      , Just $ txt " "
      , Just $ withAttr (todoKeywordAttr (view todoKeyword task)) $ txt $ view todoKeyword task
      , Just $ txt " "
      , case view priority task of
          Just p -> fmap (withAttr (priorityAttr p)) (renderPriorityWidget p)
          Nothing -> Nothing
      , Just $ txt $ view title task
      , fmap (withAttr tagAttr) (renderTagsWidget (view tags task))
      ]
   where
    renderPriorityWidget p
      | p >= 0 = Just $ txt $ T.concat ["[#", T.singleton (chr $ ord 'A' + p), "] "]
      | otherwise = Nothing

    renderTagsWidget [] = Nothing
    renderTagsWidget ts = Just $ txt $ T.concat [" :", T.intercalate ":" ts, ":"]

  renderFull task =
    vBox $
      catMaybes
        [ Just titleLine
        , Just timeFieldsLine
        , Just $ txt orgPropertiesBegin
        , Just propertiesSection
        , Just $ txt orgPropertiesEnd
        , Just $ txt "\n"
        , Just $ txtWrap $ view description task
        ]
   where
    titleLine =
      txtWrap $
        T.unwords $
          catMaybes
            [ Just $ T.replicate (view level task) "*"
            , Just $ view todoKeyword task -- TODO: colorcode them
            , view priority task >>= renderPriority
            , Just $ view title task
            , renderTags (view tags task)
            ]
    timeFieldsLine =
      txt $
        T.unwords $
          mapMaybe
            (\(field, getTime) -> fmap (renderTimeField field) (getTime task))
            [ (orgScheduledField, view scheduled)
            , (orgDeadlineField, view deadline)
            , (orgClosedField, view closed)
            ]

    renderTimeField :: TimeField -> OrgTime -> T.Text
    renderTimeField (TimeField n delim) (OrgTime t r d) =
      T.concat
        [ n
        , " "
        , T.singleton (fst delims)
        , displayOrgTime t
        , maybe "" renderRepeater r
        , maybe "" renderDelay d
        , T.singleton (snd delims)
        ]
     where
      delims :: (Char, Char)
      delims = to delim

    propertiesSection = txt $ T.intercalate "\n" (fmap (\(key, value) -> T.concat [":", key, ": ", value]) (view properties task))

    renderPriority p
      | p >= 0 = Just $ T.concat ["[#", T.singleton (chr $ ord 'A' + p), "]"]
      | otherwise = Nothing

    renderTags [] = Nothing
    renderTags ts = Just $ T.concat [":", T.intercalate ":" ts, ":"]

    displayOrgTime (Left day) = T.pack $ formatTime defaultTimeLocale orgDayFormat day
    displayOrgTime (Right utcTime) = T.pack $ formatTime defaultTimeLocale orgDayTimeFormat utcTime

    renderRepeater :: RepeatInterval -> T.Text
    renderRepeater (RepeatInterval tt v tu) =
      T.concat
        [ to tt
        , T.pack $ show v
        , T.singleton (to tu)
        ]
    renderDelay :: DelayInterval -> T.Text
    renderDelay (DelayInterval tt v tu) =
      T.concat
        [ to tt
        , T.pack $ show v
        , T.singleton (to tu)
        ]

  renderFullWithColors scheme task =
    vBox $
      catMaybes
        [ Just titleLineColored
        , Just timeFieldsLineColored
        , Just $ withAttr propertyAttr $ txt orgPropertiesBegin
        , Just propertiesSectionColored
        , Just $ withAttr propertyAttr $ txt orgPropertiesEnd
        , Just $ txt "\n"
        , Just $ withAttr descriptionAttr $ txt $ view description task
        ]
   where
    titleLineColored =
      txtWrap $
        T.unwords $
          catMaybes
            [ Just $ T.replicate (view level task) "*"
            , Just $ view todoKeyword task
            , view priority task >>= renderPriorityText
            , Just $ view title task
            , renderTagsText (view tags task)
            ]
     where
      renderPriorityText p
        | p >= 0 = Just $ T.concat ["[#", T.singleton (chr $ ord 'A' + p), "]"]
        | otherwise = Nothing

      renderTagsText [] = Nothing
      renderTagsText ts = Just $ T.concat [":", T.intercalate ":" ts, ":"]

    timeFieldsLineColored =
      withAttr timeFieldAttr $ txt $
        T.unwords $
          mapMaybe
            (\(field, getTime) -> fmap (renderTimeField field) (getTime task))
            [ (orgScheduledField, view scheduled)
            , (orgDeadlineField, view deadline)
            , (orgClosedField, view closed)
            ]

    propertiesSectionColored =
      withAttr propertyAttr $ txt $
        T.intercalate "\n" (fmap (\(key, value) -> T.concat [":", key, ": ", value]) (view properties task))

    renderTimeField :: TimeField -> OrgTime -> T.Text
    renderTimeField (TimeField n delim) (OrgTime t r d) =
      T.concat
        [ n
        , " "
        , T.singleton (fst delims)
        , displayOrgTime t
        , maybe "" renderRepeater r
        , maybe "" renderDelay d
        , T.singleton (snd delims)
        ]
     where
      delims :: (Char, Char)
      delims = to delim

    renderPriorityWidget p
      | p >= 0 = Just $ txt $ T.concat ["[#", T.singleton (chr $ ord 'A' + p), "] "]
      | otherwise = Nothing

    renderTagsWidget [] = Nothing
    renderTagsWidget ts = Just $ txt $ T.concat [" :", T.intercalate ":" ts, ":"]

    displayOrgTime (Left day) = T.pack $ formatTime defaultTimeLocale orgDayFormat day
    displayOrgTime (Right utcTime) = T.pack $ formatTime defaultTimeLocale orgDayTimeFormat utcTime

    renderRepeater :: RepeatInterval -> T.Text
    renderRepeater (RepeatInterval tt v tu) =
      T.concat
        [ to tt
        , T.pack $ show v
        , T.singleton (to tu)
        ]
    renderDelay :: DelayInterval -> T.Text
    renderDelay (DelayInterval tt v tu) =
      T.concat
        [ to tt
        , T.pack $ show v
        , T.singleton (to tu)
        ]
