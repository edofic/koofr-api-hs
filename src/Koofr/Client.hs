module Koofr.Client where

import Control.Monad.Reader
import Network.HTTP.Client 
import Network.HTTP.Types.Method
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.String (fromString)
import Data.Aeson
import Data.Maybe (fromJust)
import System.FilePath.Posix (splitFileName)
import Network.HTTP.Client.MultipartFormData

import Koofr.Class
import Koofr.Mount (mountsMounts)
import Koofr.File (fileListFiles)

data Client = Client { clientHost :: String
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

type Download = (IO ByteString, IO ()) 
type Upload = Part

instance (MonadIO m, MonadReader Client m) => MonadKoofr Download Upload m where
  mounts = do 
    resp <- clientRequest methodGet "/api/v2/mounts" noJSON
    liftIO $ mountsMounts `liftM` consumeJSON resp

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
    clientRequest methodPut
                  ("/api/v2/mounts/" ++ mountId ++ "/files/rename?path=" ++ path)
                  (Just $ object [("name", fromString name)])
    return ()

  filesCopy mountId path mountId' path' = do
    clientRequest methodPut
                  ("/api/v2/mounts/" ++ mountId ++ "/files/copy?path=" ++ path)
                  (Just $ object [ ("toMountId", fromString mountId')
                                 , ("toPath", fromString path')
                                 ])
    return ()

  filesMove mountId path mountId' path' = do
    clientRequest methodPut
                  ("/api/v2/mounts/" ++ mountId ++ "/files/move?path=" ++ path)
                  (Just $ object [ ("toMountId", fromString mountId')
                                 , ("toPath", fromString path')
                                 ])
    return ()
  

  filesDownload mountId path = do
    res <- clientRequest methodGet
                  ("/content/api/v2/mounts/" ++ mountId ++ "/files/get?path=" ++ path)
                  noJSON
    return (responseBody res, responseClose res)                  

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
