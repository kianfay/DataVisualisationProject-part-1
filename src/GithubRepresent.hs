{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module GithubRepresent where 

import Control.Monad       (mzero)
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Servant.API
import Servant.Client

data User = User {
    name :: String
} deriving (Generic, FromJSON, Show)

-- Here we define our API, outlining the structure of our HTTP requests in a abstracted way
type GitHubAPI = "users" :> Header "User-Agent" Text
                         :> Capture "username" Text 
                         :> Get '[JSON] User

-- We associate our type with this proxy type variable so that we can pass the type to Servant
gitHubAPI :: Proxy GitHubAPI
gitHubAPI = Proxy

-- Takes text(the username) and returns ClientM Text 
-- We import this function from this class into Lib.hs, and pass it to the Servant module
getUser :: Maybe Text -> Text -> ClientM User  
getUser = client gitHubAPI