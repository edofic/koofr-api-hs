module Koofr.File where

import Data.Aeson.TH
import Koofr.Internal

data FileList = FileList { fileListFiles :: [File] } deriving (Eq, Show)

data File = File { fileName :: String
                 , fileType :: String 
                 , fileModified :: Integer
                 , fileSize :: Integer
                 , fileContentType :: String
                 , hash :: Maybe String
                 } deriving (Eq, Show)

deriveJSON defaultOptions{fieldLabelModifier = (lowerCamel . drop 4)} ''File
deriveJSON defaultOptions{fieldLabelModifier = (lowerCamel . drop 8)} ''FileList
