module Koofr.Class where

import Koofr.Mount (Mount, MountId)
import Koofr.File (File)

type Name = String
type Path = String

class (Monad m) => MonadKoofr download upload m | m -> download upload where
  mounts :: m [Mount]
  mountInfo :: MountId -> m Mount

  filesInfo :: MountId -> Path -> m File
  filesList :: MountId -> Path -> m [File]
  filesNewFolder :: MountId -> Path -> Name -> m ()
  filesRemove :: MountId -> Path -> m ()
  filesRename :: MountId -> Path -> Name -> m ()
  filesCopy :: MountId -> Path -> MountId -> Path -> m ()
  filesMove :: MountId -> Path -> MountId -> Path -> m ()
  filesDownload :: MountId -> Path -> m download
  filesUpload :: MountId -> Path -> upload -> m ()