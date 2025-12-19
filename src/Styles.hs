{-# LANGUAGE OverloadedStrings #-}

module Styles (stylesheet) where

import Prelude hiding (rem, (**))
import Clay
import Data.Text.Lazy (Text)

-- | Colors
primary :: Color
primary = "#2c3e50"

secondary :: Color
secondary = "#3498db"

bg :: Color
bg = "#fafafa"

textColor :: Color
textColor = "#333"

light :: Color
light = "#fff"

-- | Generate the full stylesheet as Text
stylesheet :: Text
stylesheet = render css

-- | Main stylesheet
css :: Css
css = do
  -- Reset
  star ? do
    margin nil nil nil nil
    padding nil nil nil nil
    boxSizing borderBox

  body ? do
    fontFamily ["-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "Helvetica", "Arial"] [sansSerif]
    lineHeight (unitless 1.6)
    color textColor
    backgroundColor bg

  -- Navigation
  ".navbar" ? do
    display flex
    justifyContent spaceBetween
    alignItems center
    padding (rem 1) (rem 2) (rem 1) (rem 2)
    backgroundColor primary
    color light

  ".nav-brand" ? do
    fontSize (rem 1.5)
    fontWeight bold
    color light
    textDecoration none

  ".nav-links" ? do
    display flex
    listStyleType none
    "gap" -: "2rem"

  ".nav-links" |> li ? do
    display block

  ".nav-links" ** a ? do
    color light
    textDecoration none
    transition "opacity" (sec 0.2) ease (sec 0)

  ".nav-links" ** a # hover ? do
    opacity 0.8

  -- Main content
  ".container" ? do
    maxWidth (px 900)
    margin nil auto nil auto
    padding (rem 2) (rem 2) (rem 2) (rem 2)

  -- Hero section
  ".hero" ? do
    textAlign center
    padding (rem 4) nil (rem 4) nil
    borderBottom (px 1) solid "#eee"
    marginBottom (rem 2)

  ".hero" ** h1 ? do
    fontSize (rem 2.5)
    marginBottom (rem 0.5)
    color primary

  ".tagline" ? do
    fontSize (rem 1.2)
    color "#666"

  -- Sections
  section ? do
    marginBottom (rem 2)

  section ** h2 ? do
    color primary
    marginBottom (rem 1)
    paddingBottom (rem 0.5)
    borderBottom (px 2) solid secondary

  section ** h1 ? do
    color primary
    marginBottom (rem 1)

  -- Contact
  ".contact" ** ul ? do
    listStyleType none

  ".contact" ** li ? do
    marginBottom (rem 0.5)

  ".contact" ** a ? do
    color secondary

  -- Buttons
  ".btn" ? do
    display inlineBlock
    padding (rem 0.75) (rem 1.5) (rem 0.75) (rem 1.5)
    backgroundColor secondary
    color light
    textDecoration none
    borderRadius (px 4) (px 4) (px 4) (px 4)
    marginTop (rem 1)
    transition "background" (sec 0.2) ease (sec 0)

  ".btn" # hover ? do
    backgroundColor "#2980b9"

  ".btn-small" ? do
    display inlineBlock
    padding (rem 0.5) (rem 1) (rem 0.5) (rem 1)
    backgroundColor primary
    color light
    textDecoration none
    borderRadius (px 4) (px 4) (px 4) (px 4)
    fontSize (rem 0.9)

  ".btn-small" # hover ? do
    backgroundColor "#1a252f"

  -- Papers
  ".paper" ? do
    padding (rem 1.5) (rem 1.5) (rem 1.5) (rem 1.5)
    backgroundColor light
    borderRadius (px 8) (px 8) (px 8) (px 8)
    marginBottom (rem 1)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithSpread nil (px 2) (px 4) nil

  ".paper" ** h3 ? do
    color primary
    marginBottom (rem 0.5)

  ".paper-meta" ? do
    color "#666"
    fontStyle italic
    marginBottom (rem 0.5)

  -- Projects
  ".projects" ** ul ? do
    listStyleType disc
    marginLeft (rem 1.5)

  ".projects" ** a ? do
    color secondary
