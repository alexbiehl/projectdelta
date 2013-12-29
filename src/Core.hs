{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Core(
  CoreM,
  CoreEnv,
  mkCoreEnv,
  closeCoreEnv,
  withCoreEnv,
  runCoreM,
  users,
  userPermissions,
  addUser
  ) where

import Data.Acid
import Data.Typeable
import Data.SafeCopy
import Data.Maybe
import Data.Monoid

import Control.Exception
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State hiding (state)

import Types
import qualified User
import qualified Group

data CoreState = CoreState {
    csUsers  :: Users
  , csGroups :: Groups
  } deriving (Typeable, Show)

deriveSafeCopy 0 'base ''CoreState

data CoreEnv = CoreEnv {
    coreState :: AcidState CoreState
  }

newtype CoreM a = CoreM { unCoreM :: ReaderT CoreEnv IO a }
  deriving (Functor, Applicative, Monad, MonadReader CoreEnv, MonadIO)

runCoreM :: CoreEnv -> CoreM a -> IO a
runCoreM env m = runReaderT (unCoreM m) env

initialState :: CoreState
initialState = CoreState User.empty Group.empty

lookupPermissions :: Email -> CoreState -> Maybe Permissions
lookupPermissions email state = do
    groups <- User.groups email (csUsers state)
    let perms = (flip Group.permissions allGroups) <$> Group.groupIdsToList groups
    return $ mconcat $ catMaybes perms
  where
    allGroups = csGroups state

users' :: Query CoreState [(Email, GroupIds)]
users' = undefined

userPermissions' :: Email -> Query CoreState (Maybe Permissions)
userPermissions' email = lookupPermissions email <$> ask

addUser' :: Email -> GroupIds -> Update CoreState ()
addUser' email gids = do
  users  <- User.add' email gids <$> gets csUsers
  groups <- Group.add'' gids <$> gets csGroups
  put $ CoreState users groups

makeAcidic ''CoreState ['users', 'addUser', 'userPermissions']

users :: CoreM [(Email, GroupIds)]
users = do
  state <- coreState <$> ask
  liftIO $ query state Users'

addUser :: Email -> GroupIds -> CoreM ()
addUser email gids = do
  state <- coreState <$> ask
  liftIO $ update state (AddUser' email gids)

userPermissions :: Email -> CoreM (Maybe Permissions)
userPermissions email = do
  state <- coreState <$> ask
  liftIO $ query state (UserPermissions' email)

mkCoreEnv :: IO CoreEnv
mkCoreEnv = do
  acid <- openLocalState initialState
  return $ CoreEnv acid

closeCoreEnv :: CoreEnv -> IO ()
closeCoreEnv = closeAcidState . coreState

withCoreEnv :: (CoreEnv -> IO ()) -> IO ()
withCoreEnv f = bracket mkCoreEnv closeCoreEnv f
