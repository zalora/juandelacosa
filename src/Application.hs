{-# LANGUAGE OverloadedStrings #-}

module Application
(
  app
) where

import Data.ByteString.Base64 (encode)
import Data.Pool (Pool, withResource)
import Database.MySQL.Simple (Connection, execute)
import Network.HTTP.Types (status200, badRequest400, Header)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Application, requestHeaders, responseLBS, Response)
import System.Entropy (getEntropy)
import System.IO (stderr)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.CaseInsensitive as CI


app :: Pool Connection -> Application
app p request respond = do
  let
    headers = requestHeaders request
    from = readHeader "From" headers
  case from of
    Just login -> apiResetPassword p login >>= respond
    Nothing -> respond $ responseLBS
              badRequest400
              [(hContentType, "text/plain")]
              "Missing the From header"


apiResetPassword :: Pool Connection -> BS.ByteString -> IO Response
apiResetPassword p login = withResource p $
  \c -> do
     blab ["SET PASSWORD FOR '", login, "'@'%'"]
     password <- BS.takeWhile (/= '=') . encode <$> getEntropy 12
     _ <- execute c "SET PASSWORD FOR ?@'%' = PASSWORD(?)"  [ login, password ]
     return $ responseLBS
              status200
              [(hContentType, "text/plain")]
              (LBS.fromStrict password)


readHeader :: BS.ByteString -> [Header] -> Maybe BS.ByteString
readHeader h = lookup (CI.mk h)


blab :: [BS.ByteString] -> IO ()
blab = BS.hPutStrLn stderr . BS.concat

