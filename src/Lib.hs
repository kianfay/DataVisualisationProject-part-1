-- We need the OverloadedStrings extension so that "kianfay" below 
-- is read as a Text type instead of [Char] (see what happens when
-- we remove this extension). The lambdaCase allows
-- us to use the case statement in the way shown below.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings    #-}

module Lib
    ( someFunc
    ) where

import GithubRepresent ( getUser )
import Servant.Client
import Network.HTTP.Client          (newManager)
import Network.HTTP.Client.TLS      (tlsManagerSettings)
import Data.Text ( Text )

-- This is the function called by Main.hs
someFunc :: IO ()
someFunc = doGithubReq "kianfay"

-- This function accepts a useranme of type Text, and performs
-- the Servant functionality
doGithubReq :: Text -> IO()
doGithubReq username =  let env :: IO Servant.Client.ClientEnv
                            env = do
                                manager <- newManager tlsManagerSettings
                                -- Here we define a ClientEnv smart using the smart constructor, mkClientEnv
                                return $ Servant.Client.mkClientEnv manager (Servant.Client.BaseUrl {baseUrlScheme = Servant.Client.Http, baseUrlHost = "api.github.com", baseUrlPort = 80, baseUrlPath = ""})
                        -- m >>= k suggests "feed the result of computation m to the function k" - from StackOverflow
                        -- We could also put case after in, and we would not write case with a \.
                        in (Servant.Client.runClientM (GithubRepresent.getUser (Just "Visualisation-App") username) =<< env) >>= \case
                            Left err -> do
                                -- The show function is passed an instance of the class Show (err or res in this case, being an 
                                -- instance of ClientError or Data.Text.Internal.Text) and returns a string representation
                                putStrLn ("We have an ERROR -- " ++ show err)
                            Right res -> do
                                putStrLn ("We made a successful request and got the response -- " ++ show res)
