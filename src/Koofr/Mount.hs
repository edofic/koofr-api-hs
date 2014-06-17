module Koofr.Mount where 

import Data.Aeson.TH
import Koofr.Internal
import Data.Char

type MountId = String

data Mounts = Mounts { mountsMounts :: [Mount] } deriving (Eq, Show)

data Mount = Mount { mountId :: MountId
                   , mountName :: String
                   , mountType :: String
                   , mountOnline :: Bool
                   , mountOwner :: MountUser
                   , mountUsers :: [MountUser]
                   , mountGroups :: [MountUser]
                   , mountIsShared :: Bool
                   , mountPermissions :: Permissions
                   , mountSpaceTotal :: Integer
                   , mountSpaceUsed :: Integer
                   , mountVersion :: Integer
                   , mountIsPrimary :: Bool
                   , mountCanWrite :: Bool
                   , mountCanUpload :: Bool
                   , mountOverQuota :: Bool
                   } deriving (Eq, Show)

data MountUser = MountUser { mountUserId :: String 
                           , mountUserName :: String
                           , mountUserEmail :: Maybe String
                           , mountUserPermissions :: Permissions
                           , mountUserIsGroup :: Maybe Bool
                           } deriving (Eq, Show)

data Permissions = Permissions { permissionRead :: Bool
                               , permissionOwner :: Bool
                               , permissionMount :: Bool
                               , permissionCreate_Receiver :: Bool
                               , permissionComment :: Bool
                               , permissionWrite :: Bool
                               , permissionCreate_Link :: Bool
                               } deriving (Eq, Show)                                                      

deriveJSON defaultOptions{fieldLabelModifier = (map toUpper . drop 10)} ''Permissions
deriveJSON defaultOptions{fieldLabelModifier = (lowerCamel . drop 9)} ''MountUser
deriveJSON defaultOptions{fieldLabelModifier = (lowerCamel . drop 5)} ''Mount
deriveJSON defaultOptions{fieldLabelModifier = (lowerCamel . drop 6)} ''Mounts
