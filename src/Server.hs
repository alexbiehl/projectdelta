{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Server(runServer) where

import Control.Monad.Trans

import Web.Scotty.Trans
import Data.Aeson.Types
import Data.Text.Lazy (Text)

import qualified Core

type ProjectDeltaAction = ActionT Text Core.CoreM

userEmails :: ProjectDeltaAction ()
userEmails = do
  users <- lift Core.users
  json $ object ["result" .= users]

userPermissions :: ProjectDeltaAction ()
userPermissions = do
  email <- param "email"
  permissions <- lift $ Core.userPermissions email
  let jsonPermissions = emptyArray
  json $ object ["result" .= jsonPermissions]

runServer :: Core.CoreEnv -> IO ()
runServer env = scottyT 8080 runCore runCore $ do
    get "/users" userEmails
    get "/users/:email/permissions" userPermissions
  where
    runCore = Core.runCoreM env
