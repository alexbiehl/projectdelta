{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.SafeCopy (deriveSafeCopy, base)
import Data.Typeable
import Data.Map.Strict as HM
import Data.Set as HS
import Data.Text

data Permission = Permission deriving (Eq, Ord, Show, Typeable)

type Permissions = HS.Set Permission

type Email = Text

type GroupId = Text

type GroupIds = HS.Set GroupId

data User = User { userGroups :: GroupIds } deriving (Show, Typeable)

newtype Users = Users { allUsers :: HM.Map Email User } deriving (Typeable, Show)

newtype Groups = Groups { allGroups :: HM.Map GroupId Permissions } deriving (Typeable, Show)

deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''Users
deriveSafeCopy 0 'base ''Groups
deriveSafeCopy 0 'base ''Permission
