
module User(
    empty,
    emails,
    add,
    add',
    delete,
    groups,
    assignGroup,
    revokeGroup
  ) where

import qualified Data.Map.Strict as HM
import qualified Data.Set as HS

import Types

empty :: Users
empty = Users (HM.empty)

add :: Email -> Users -> Users
add em = Users . HM.insert em (User HS.empty) . allUsers

add' :: Email -> GroupIds -> Users -> Users
add' em gid users = assignGroups em gid (add em users)

emails :: Users -> [Email]
emails = HM.keys . allUsers

groups :: Email -> Users -> Maybe GroupIds
groups em usrs = fmap userGroups $ HM.lookup em (allUsers usrs)

delete :: Email -> Users -> Users
delete em = Users . HM.delete em . allUsers

assignGroup :: Email -> GroupId -> Users -> Users
assignGroup em gid = assignGroups em (HS.singleton gid)

assignGroups :: Email -> GroupIds -> Users -> Users
assignGroups em gids = Users . HM.adjust adjust em . allUsers
  where
    adjust (User gids') = User (HS.union gids' gids)

revokeGroup :: Email -> GroupId -> Users -> Users
revokeGroup em gid = Users . HM.adjust adjust em . allUsers
  where
    adjust (User gids) = User (HS.delete gid gids)