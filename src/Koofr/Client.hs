module Koofr.Client
( Client(..) , runClient
, Download , Upload
, Name, path
, createNewAuthToken
, createDefaultManager
, mounts
, mountInfo
, filesInfo
, filesList
, filesNewFolder
, filesRemove
, filesRename
, filesCopy
, filesMove
, filesDownload
, filesUpload
) where

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Maybe (fromJust)
import           Data.String (fromString)
import           Control.Monad.Reader
import           Network.HTTP.Client 
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Method
import           System.FilePath.Posix (splitFileName)

import Koofr.Mount 
import Koofr.File 

type Host = String
data Client = Client { clientHost :: Host
                     , clientToken :: String
                     , clientManager :: Manager
                     } 

runClient :: Client -> ReaderT Client m a -> m a
runClient = flip runReaderT

tokenHeader token = ("Authorization", fromString $ "Token token=" ++ token)

clientRequest method path body = do
  Client host token manager <- ask
  liftIO $ do 
    req'' <- parseUrl $ host ++ path
    let contentType = maybe [] (const [("Content-Type", "application/json")]) body
        req' = req'' { method = method
                     , requestHeaders = tokenHeader token : contentType
                     }
        req = maybe req' (\b -> req' { requestBody = RequestBodyLBS $ encode b }) body
    responseOpen req manager

noJSON :: Maybe Value
noJSON = Nothing

consumeJSON response = liftIO $ 
  do lbs <- brConsume (responseBody response)
     responseClose response
     return $ fromJust $ decode $ L.fromChunks lbs

createNewAuthToken :: Manager -> Host -> String -> String -> IO (Maybe String)
createNewAuthToken manager host email password = do
  req' <- parseUrl $ host ++ "/token"
  let req = req' { method = methodPost
                 , requestHeaders = ("Content-Type", "application/json") : requestHeaders req'
                 , requestBody = RequestBodyLBS $ encode b 
                 }
      b   = object [("email", fromString email), ("password", fromString password)]
  res <- httpLbs req manager
  return $ decode (responseBody res) >>= parseMaybe (.: "token")

createDefaultManager :: IO Manager 
createDefaultManager = newManager tlsManagerSettings

type Download = (IO ByteString, IO ()) 
type Upload = Part
type Name = String
type Path = String 


mounts :: (MonadIO m, MonadReader Client m) =>  m [Mount]
mounts = do 
  resp <- clientRequest methodGet "/api/v2/mounts" noJSON
  liftIO $ mountsMounts `liftM` consumeJSON resp

mountInfo :: (MonadIO m, MonadReader Client m) =>  MountId -> m Mount
mountInfo mountId = do 
  resp <- clientRequest methodGet ("/api/v2/mounts/" ++ mountId) noJSON
  consumeJSON resp

filesInfo :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> m File
filesInfo mountId path = do 
  resp <- clientRequest methodGet ("/api/v2/mounts/" ++ mountId ++ "/files/info?path=" ++ path)
                noJSON
  consumeJSON resp

filesList :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> m [File]
filesList mountId path = do 
  resp <- clientRequest methodGet ("/api/v2/mounts/" ++ mountId ++ "/files/list?path=" ++ path)
                noJSON
  fileListFiles `liftM` consumeJSON resp

filesNewFolder :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> Name -> m ()
filesNewFolder mountId path name = do
  clientRequest methodPost 
                ("/api/v2/mounts/" ++ mountId ++ "/files/folder?path=" ++ path)
                (Just $ object [("name", fromString name)])
  return ()

filesRemove :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> m ()
filesRemove mountId path = do
  clientRequest methodDelete
                ("/api/v2/mounts/" ++ mountId ++ "/files/remove?path=" ++ path)
                noJSON
  return ()

filesRename :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> Name -> m ()
filesRename mountId path name = do
  clientRequest methodPut
                ("/api/v2/mounts/" ++ mountId ++ "/files/rename?path=" ++ path)
                (Just $ object [("name", fromString name)])
  return ()

filesCopy :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> MountId -> Path -> m ()
filesCopy mountId path mountId' path' = do
  clientRequest methodPut
                ("/api/v2/mounts/" ++ mountId ++ "/files/copy?path=" ++ path)
                (Just $ object [ ("toMountId", fromString mountId')
                               , ("toPath", fromString path')
                               ])
  return ()

filesMove :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> MountId -> Path -> m ()
filesMove mountId path mountId' path' = do
  clientRequest methodPut
                ("/api/v2/mounts/" ++ mountId ++ "/files/move?path=" ++ path)
                (Just $ object [ ("toMountId", fromString mountId')
                               , ("toPath", fromString path')
                               ])
  return ()


filesDownload :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> m Download
filesDownload mountId path = do
  res <- clientRequest methodGet
                ("/content/api/v2/mounts/" ++ mountId ++ "/files/get?path=" ++ path)
                noJSON
  return (responseBody res, responseClose res)                  

filesUpload :: (MonadIO m, MonadReader Client m) =>  MountId -> Path -> Upload -> m ()
filesUpload mountId path part = do
  Client host token manager <- ask
  let (dirname, fileName) = splitFileName path
      url = "/content/api/v2/mounts/" ++ mountId ++ "/files/put?path=" ++ dirname ++ "&filename=" ++ fileName
  req'' <- liftIO $ parseUrl $ host ++ url
  let req' = req'' { requestHeaders = [tokenHeader token] }
      part' = part { partName = "file"
                   , partFilename = Just fileName 
                   }
  req <- formDataBody [part'] req'
  liftIO $ httpNoBody req manager
  return ()
