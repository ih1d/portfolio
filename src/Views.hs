{-# LANGUAGE OverloadedStrings #-}

module Views
  ( homePage
  , resumePage
  , papersPage
  , basePage
  ) where

import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Lucid

import Styles (stylesheet)

-- | Base page template with common structure
basePage :: Monad m => Text -> HtmlT m () -> HtmlT m ()
basePage title content = do
  doctype_
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      title_ (toHtml title)
      style_ (LT.toStrict stylesheet)
    body_ $ do
      nav_ [class_ "navbar"] $ do
        a_ [href_ "/", class_ "nav-brand"] "Isaac Lopez"
        ul_ [class_ "nav-links"] $ do
          li_ $ a_ [href_ "/"] "Home"
          li_ $ a_ [href_ "/resume"] "Resume"
          li_ $ a_ [href_ "/papers"] "Papers"
      main_ [class_ "container"] content

-- | Home page with personal info
homePage :: Html ()
homePage = basePage "Isaac Lopez - Portfolio" $ do
  section_ [class_ "hero"] $ do
    h1_ "Isaac Hiram Lopez Diaz"
    p_ [class_ "tagline"] "Software Engineer & Researcher"

  section_ [class_ "about"] $ do
    h2_ "About Me"
    p_ "Welcome to my portfolio. I am a software engineer passionate about \
       \functional programming and research."

  section_ [class_ "contact"] $ do
    h2_ "Contact"
    ul_ $ do
      li_ $ do
        "Email: "
        a_ [href_ "mailto:isaac.lopez@upr.edu"] "isaac.lopez@upr.edu"

  section_ [class_ "projects"] $ do
    h2_ "Projects"
    ul_ $ do
      li_ $ a_ [href_ "https://git.ih1d.com/blue", target_ "_blank"] "blue"

-- | Resume page
resumePage :: Html ()
resumePage = basePage "Resume - Isaac Lopez" $ do
  section_ $ do
    h1_ "Resume"
    p_ "View or download my resume below."
    a_ [href_ "/static/docs/resume.pdf", class_ "btn", target_ "_blank"]
       "Download Resume (PDF)"

  section_ [class_ "resume-preview"] $ do
    h2_ "Preview"
    iframe_ [ src_ "/static/docs/resume.pdf"
            , width_ "100%"
            , height_ "800"
            , style_ "border: 1px solid #ccc; border-radius: 4px;"
            ] ""

-- | Papers page
papersPage :: Html ()
papersPage = basePage "Papers - Isaac Lopez" $ do
  section_ $ do
    h1_ "Research Papers"
    ul_ [class_ "papers-list"] $ do
      li_ $ a_ [href_ "/static/docs/papers/reflec.pdf", target_ "_blank"]
            "A Reflective Language for the Analysis of Dataflow Semantics"
