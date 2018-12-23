{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TypeOperators  #-}

module Lib
    ( myRun
    ) where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

type FtvAPI = QueryParam "search" Query
           :> Get '[JSON] Obj

ftvAPI :: Proxy FtvAPI
ftvAPI = Proxy

myRun :: IO ()
myRun = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve ftvAPI server

server :: Server FtvAPI
server = getTestObj

getTestObj :: Maybe Query -> Handler Obj
getTestObj (Just q) = pure $ Obj q
getTestObj Nothing  = pure $ Obj "You don't deserve me"

data Obj = Obj { coucou :: String } deriving (Show, Generic)

instance ToJSON Obj
instance FromJSON Obj

type Query = String
