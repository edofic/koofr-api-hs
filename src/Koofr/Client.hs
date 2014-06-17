module Koofr.Client where

import Control.Monad.Reader
import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Data.ByteString (ByteString)
import Data.Conduit (($$+-), ResumableSource)
import Data.Conduit.Binary (sinkLbs)
import Data.String (fromString)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Aeson
import System.FilePath.Posix (splitFileName)
import Network.HTTP.Client.MultipartFormData

import Koofr.Class
import Koofr.Mount (mountsMounts)
import Koofr.File (fileListFiles)

data Client = Client { clientHost :: String
                     , clientToken :: String
                     , clientManager :: Manager
                     } 

runClient :: (MonadResource m) => ReaderT Client m a -> Client -> m a
runClient = runReaderT

tokenHeader token = ("Authorization", fromString $ "Token token=" ++ token)

clientRequest method path body = do
  Client host token manager <- ask
  req'' <- parseUrl $ host ++ path
  let contentType = maybe [] (const [("Content-Type", "application/json")]) body
      req' = req'' { method = method
                   , requestHeaders = tokenHeader token : contentType
                   }
      req = maybe req' (\b -> req' { requestBody = RequestBodyLBS $ encode b }) body
  http req manager

noJSON :: Maybe Value
noJSON = Nothing

consumeJSON response = do
  let body = responseBody response
  bs <- body $$+- sinkLbs
  let Just res = decode bs
  return res  

type Download m = Response (ResumableSource m ByteString) 
type Upload = Part

instance (MonadResource m, MonadReader Client m) => MonadKoofr (Download m) Upload m where
  mounts = do 
    resp <- clientRequest methodGet "/api/v2/mounts" noJSON
    mountsMounts `liftM` consumeJSON resp

  mountInfo mountId = do 
    resp <- clientRequest methodGet ("/api/v2/mounts/" ++ mountId) noJSON
    consumeJSON resp


  filesInfo mountId path = do 
    resp <- clientRequest methodGet ("/api/v2/mounts/" ++ mountId ++ "/files/info?path=" ++ path)
                  noJSON
    consumeJSON resp

  filesList mountId path = do 
    resp <- clientRequest methodGet ("/api/v2/mounts/" ++ mountId ++ "/files/list?path=" ++ path)
                  noJSON
    fileListFiles `liftM` consumeJSON resp
  
  filesNewFolder mountId path name = do
    clientRequest methodPost 
                  ("/api/v2/mounts/" ++ mountId ++ "/files/folder?path=" ++ path)
                  (Just $ object [("name", fromString name)])
    return ()

  filesRemove mountId path = do
    clientRequest methodDelete 
                  ("/api/v2/mounts/" ++ mountId ++ "/files/remove?path=" ++ path)
                  noJSON
    return ()

  filesRename mountId path name = do
    clientRequest methodDelete 
                  ("/api/v2/mounts/" ++ mountId ++ "/files/rename?path=" ++ path)
                  (Just $ object [("name", fromString name)])
    return ()

  filesCopy mountId path mountId' path' = do
    clientRequest methodDelete 
                  ("/api/v2/mounts/" ++ mountId ++ "/files/copy?path=" ++ path)
                  (Just $ object [ ("toMountId", fromString mountId')
                                 , ("toPath", fromString path')
                                 ])
    return ()

  filesMove mountId path mountId' path' = do
    clientRequest methodDelete 
                  ("/api/v2/mounts/" ++ mountId ++ "/files/move?path=" ++ path)
                  (Just $ object [ ("toMountId", fromString mountId')
                                 , ("toPath", fromString path')
                                 ])
    return ()
  

  filesDownload mountId path = 
    clientRequest methodGet
                  ("/content/api/v2/mounts/" ++ mountId ++ "/files/get/?path=" ++ path)
                  noJSON


  filesUpload mountId path part = do
    Client host token manager <- ask
    let (dirname, fileName) = splitFileName path
        url = "/content/api/v2/mounts/" ++ mountId ++ "/files/put?path=" ++ dirname ++ "&filename=" ++ fileName
    req'' <- parseUrl $ host ++ url
    let req' = req'' { requestHeaders = [tokenHeader token] }
    req <- formDataBody [part] req'
    http req manager
    return ()
