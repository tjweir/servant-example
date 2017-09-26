{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Prelude ()
import Prelude.Compat

import Control.Monad.Except
import Control.Monad.Reader
--import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
--import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

-- API endpoints
type UserAPI1 = "users" :> Get '[JSON] [User]
type UserAPI2 = "users" :> Get '[JSON] [User]
  :<|> "albert" :> Get '[JSON] User
  :<|> "isaac"  :> Get '[JSON] User


-- we are going to return this
data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User



-- data
users1 :: [User]
users1 =
  [ User "Isaac Newton"    372 "isaac@newton.co.uk" (fromGregorian 1683  3 1)
  , User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
  ]

isaac :: User
isaac = User "Isaac" 372 "i@newton.co.uk" (fromGregorian 1683  3 1)

albert :: User
albert = User "albert" 136 "a@einstein.co.uk" (fromGregorian 1683  3 1)

users2 :: [User]
users2 = [isaac, albert]



-- type of a server
server1 :: Server UserAPI1
server1 = return users1

server2 :: Server UserAPI2
server2 = return users2
 :<|> return albert
 :<|> return isaac

userAPI1 :: Proxy UserAPI1
userAPI1 = Proxy

userAPI2 :: Proxy UserAPI2
userAPI2 = Proxy
-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Application
app1 = serve userAPI1 server1

app2 :: Application
app2 = serve userAPI2 server2

main :: IO ()
main = run 8081 app2
