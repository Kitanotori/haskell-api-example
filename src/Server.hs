{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

module Server ( runServer ) where

import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant

type RestAPI (name :: Symbol) a i = name :>
  (                         Get '[JSON] [a]                               -- GET /<name>
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent                        -- POST /<name>
  :<|> Capture "id" i    :> Get '[JSON] a                                 -- GET /<name>/:id
  :<|> Capture "id" i    :> ReqBody '[JSON] a :> Put '[JSON] NoContent    -- PUT /<name>/:id
  :<|> Capture "id" i    :> ReqBody '[JSON] a :> Patch '[JSON] NoContent  -- PATCH /<name>/:id
  :<|> Capture "id" i    :> Delete '[JSON] NoContent                      -- DELETE /<name>/:id
  )

createRestEndpoint :: Handler [a]   -- GET /<name>
  -> (a -> Handler NoContent)       -- POST /<name>
  -> (i -> Handler a)               -- GET /<name>/:id
  -> (i -> a -> Handler NoContent)  -- PUT /<name>/:id
  -> (i -> a -> Handler NoContent)  -- PATCH /<name>/:id
  -> (i -> Handler NoContent)       -- DELETE /<name>/:id
  -> Server (RestAPI name a i)      
createRestEndpoint list post get put patch delete =
  list :<|> post :<|> get :<|> put :<|> patch :<|> delete

type DbId = Int

data User = User
  { userId        :: DbId
  , userFirstname :: String
  , userLastname  :: String
  } deriving (Eq, Generic, Show)

instance FromJSON User
instance ToJSON   User
-- $(deriveJSON defaultOptions ''User)

data Message = Message
  { messageId      :: DbId
  , messageUserId :: Int
  , messageBody :: String
  } deriving (Eq, Generic, Show)

instance FromJSON Message
instance ToJSON   Message
-- $(deriveJSON defaultOptions ''Message)

type UserAPI = RestAPI "users" User DbId
userEndpoint :: Server UserAPI
userEndpoint = createRestEndpoint
  (return [ User 1 "Isaac" "Newton", User 2 "Albert" "Einstein" ])
  (\_data -> return NoContent)
  (\id -> return $
      if id == 0
      then User 0 "John" "Smith"
      else User 111 "John" "Doe"
  )
  (\id _data -> return NoContent)
  (\id _data -> return NoContent)
  (\id -> return NoContent)

type MessageAPI = RestAPI "messages" Message DbId
messageEndpoint :: Server MessageAPI
messageEndpoint = createRestEndpoint
  (return [ Message 0 0 "Message 0", Message 111 111 "Message 111" ])
  (\_data -> return NoContent)
  (\id -> return $
      if id == 0
      then Message 0 0 "Message 0"
      else Message 111 111 "Message 111"
  )
  (\id _data -> return NoContent)
  (\id _data -> return NoContent)
  (\id -> return NoContent)

type API = UserAPI :<|> MessageAPI
api :: Proxy API
api = Proxy

port = 8080

runServer :: IO ()
runServer = do
  putStrLn ("Starting server" ++ " http://localhost:" ++ show port)
  run port . serve api $ userEndpoint :<|> messageEndpoint
