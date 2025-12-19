{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant
import Servant.HTML.Lucid (HTML)
import Lucid (Html)

import Views (homePage, resumePage, papersPage)

-- | Main API type
type PortfolioAPI =
       Get '[HTML] (Html ())                          -- Home page at /
  :<|> "resume" :> Get '[HTML] (Html ())              -- Resume page at /resume
  :<|> "papers" :> Get '[HTML] (Html ())              -- Papers page at /papers
  :<|> "static" :> Raw                                -- Static files at /static

portfolioAPI :: Proxy PortfolioAPI
portfolioAPI = Proxy

-- | Server implementation
server :: FilePath -> Server PortfolioAPI
server staticDir =
       return homePage
  :<|> return resumePage
  :<|> return papersPage
  :<|> serveDirectoryWebApp staticDir
