module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BL (fromStrict, toStrict)
import Data.Functor.Alt ((<!>))
import Data.Proxy (Proxy (..))
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.LocalTime
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Media qualified as M
import Servant.API
  ( Accept (..)
  , Capture
  , Get
  , MimeRender (..)
  , MimeUnrender (..)
  , (:>)
  )
import Servant.Client
  ( ClientError
  , ClientM
  , client
  , mkClientEnv
  , parseBaseUrl
  )
import Servant.Client.Internal.HttpClient (runClientM)
import System.Exit (die)
import System.Posix.ByteString
  ( createSymbolicLink
  , fileExist
  , removeLink
  )
import System.Posix.Env.ByteString (getArgs)
import System.Posix.User.ByteString
  ( UserEntry (homeDirectory)
  , getUserEntryForName
  )
import Prelude

type Year = T.Text

type Month = T.Text

type HomeDir = ByteString

type FileDest = ByteString

main :: IO ()
main = do
  (homeDirectory -> homeDir) <- getArgs >>= firstArg >>= getUserEntryForName
  tz <- getCurrentTimeZone
  (year, month) <- mkYearMonth . utcToLocalTime tz <$> getCurrentTime
  main' homeDir year month

-- mapM_ (uncurry (main' homeDir))
--      [(year, month) | year <- ["2024", "2025", "2026"]
--                     , month <- ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]]

main' :: HomeDir -> Year -> Month -> IO ()
main' homeDir year month = do
  let fileDest = mkFileDest year month homeDir
      symDest = mkSymDest homeDir
  putStrLn [i|Downloading wallpaper for #{month} #{year}|]

  newImgExists <- fileExist fileDest
  if newImgExists
    then do
      putStrLn [i|Warning: #{fileDest} already exists not downloading...|]
    else do
      reqResult <- downloadImage year month
      case reqResult of
        Left err -> putStrLn [i|Error: #{err}|]
        Right imageData -> saveImage fileDest imageData

  swapCurrentWp fileDest symDest

mkRelFileName :: Year -> Month -> ByteString
mkRelFileName year month = [i|wallpaper-#{month}-#{year}.jpg|]

mkFileDest :: Year -> Month -> HomeDir -> FileDest
mkFileDest year month homeDir = [i|#{homeDir}/Pictures/#{relFileName}|]
 where
  relFileName = mkRelFileName year month

mkSymDest :: HomeDir -> FileDest
mkSymDest homeDir = [i|#{homeDir}/Pictures/wallpaper.jpg|]

downloadImage :: Year -> Month -> IO (Either ClientError ByteString)
downloadImage year month = do
  manager <- newManager tlsManagerSettings
  baseUrl <- parseBaseUrl "kriegs.net"
  flip runClientM (mkClientEnv manager baseUrl) $ requestImage year month

firstArg :: [a] -> IO a
firstArg [] =
  die "Missing argument: expected username in first argument, but received none"
firstArg (user : _) = pure user

mkYearMonth :: LocalTime -> (Year, Month)
mkYearMonth lt = do
  let fmtDate fmtStr = T.pack $ formatTime defaultTimeLocale fmtStr lt
      yearStr = fmtDate "%Y"
      monthStr = fmtDate "%B"
  (yearStr, monthStr)

requestImage :: Year -> Month -> ClientM ByteString
requestImage year month = do
  let seps :: [T.Text] = ["-", "_"] -- Weird change he made
  -- let imageUrl =
  --       https "www.kriegs.net"
  --         /: "work"
  --         /: year
  --         /: "wallpapers"
  --         /: [i|kriegs_#{year}_#{month}_4K_3840x2160_calendar.jpg|]
  let imageUrls =
        fmap (\sep -> [i|kriegs_#{year}_#{month}_4K_3840x2160#{sep}calendar.jpg|]) seps
  let reqs = fmap (requestImage' year) imageUrls
  foldr1 (<!>) reqs

data JPEG

instance Accept JPEG where
  contentType _ = "image" M.// "jpeg"

instance MimeRender JPEG ByteString where
  mimeRender _ = BL.fromStrict

instance MimeUnrender JPEG ByteString where
  mimeUnrender _ = Right . BL.toStrict

type KriegsAPI =
  "work"
    :> Capture "year" Year
    :> "wallpapers"
    :> Capture "slug" T.Text
    :> Get '[JPEG] ByteString

api :: Proxy KriegsAPI
api = Proxy

requestImage' :: Year -> T.Text -> ClientM ByteString
requestImage' = client api

swapCurrentWp :: (MonadIO m) => FileDest -> FileDest -> m ()
swapCurrentWp wallpaperDest symDest = do
  liftIO $ putStrLn [i|symlink #{symDest} -> #{wallpaperDest}|]
  symExists <- liftIO $ fileExist symDest
  when symExists do
    liftIO $ removeLink symDest
  liftIO $ createSymbolicLink wallpaperDest symDest

saveImage :: (MonadIO m) => FileDest -> ByteString -> m ()
saveImage fileDest imgData = do
  liftIO $ putStrLn [i|write #{BS.length imgData} bytes to #{fileDest}|]
  liftIO $ BS.writeFile (BS.unpack fileDest) imgData
