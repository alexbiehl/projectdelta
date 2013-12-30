
module Group(
    empty,
    add,
    add',
    add'',
    remove,
    permissions,
    permissions',
    assignPermission,
    revokePermission,
    groupIdsToList
  ) where

import Data.Monoid
import qualified Data.Map.Strict as HM
import qualified Data.Set as HS

import Types

groupIdsToList :: GroupIds -> [GroupId]
groupIdsToList = HS.toList

empty :: Groups
empty = Groups (HM.empty)

add'' :: GroupIds -> Groups -> Groups
add'' gids grps = HS.foldl (\grps gid -> Group.add' gid grps) grps gids

add :: GroupId -> Permissions -> Groups -> Groups
add gid perms = Groups . HM.insertWith mappend gid perms . allGroups

add' :: GroupId -> Groups -> Groups
add' gid = add gid HS.empty

remove :: GroupId -> Groups -> Groups
remove gid = Groups . HM.delete gid . allGroups

permissions' :: GroupIds -> Groups -> Permissions
permissions' gids grps =
  HS.unions $ map (flip permissions grps) $ groupIdsToList gids

permissions :: GroupId -> Groups -> Permissions
permissions gid = HM.findWithDefault HS.empty gid . allGroups

assignPermission :: GroupId -> Permission -> Groups -> Groups
assignPermission gid perm = add gid (HS.singleton perm)

revokePermission :: GroupId -> Permission -> Groups -> Groups
revokePermission gid perm = Groups. HM.adjust adjust gid . allGroups
  where
    adjust = HS.delete perm