{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module GithubRepresent where 

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

data Org = Org {
    public_repos :: Int
} deriving (Generic, FromJSON, Show)

data Repo = Repo {
    name :: Text
} deriving (Generic, FromJSON, Show)

-- Here we define our API, outlining the structure of our HTTP requests in a abstracted way
-- The first one gets the Organization endpoint of a given organization, so we can get the number of public repos.
-- The second one gets the repos of a given organization.
type GitHubAPI =     Header "User-Agent" Text
                    :> "orgs"
                    :> Capture "organization" Text 
                    :> Get '[JSON] Org
                :<|> Header "User-Agent" Text
                    :> "orgs"
                    :> Capture "organization" Text 
                    :> "repos"
                    :> QueryParam "per_page" Int
                    :> Get '[JSON] [Repo]

-- We associate our type with this proxy type variable so that we can pass the type to Servant
gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

-- Define the types of the different api functions (to translate gitHubAPI to Haskell functions)
type Organization = Text
type UserAgentHeader = Text
type PerPage = Int

getOrg :: Maybe UserAgentHeader -> Organization -> ClientM Org 

getRepos :: Maybe UserAgentHeader -> Organization -> Maybe PerPage -> ClientM [Repo]

getOrg :<|> getRepos = client gitHubAPI