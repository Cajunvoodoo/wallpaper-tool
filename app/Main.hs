module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.String.Interpolate (i)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.Time.LocalTime
import Network.HTTP.Req
import System.Exit (die)
import System.Posix.ByteString
  ( fileExist
  , getUserEntryForName
  , homeDirectory
  , rename
  )
import Prelude

type Year = T.Text
type Month = T.Text

main :: IO ()
main = do
  tz <- getCurrentTimeZone
  (year, month) <- mkYearMonth . utcToLocalTime tz <$> getCurrentTime
  newImgExists <- alreadyExists year month
  if newImgExists
    then
      die
        [i|Error: kriegs_#{year}_#{month}_4K_3840x2160_calendar.jpg already exists, skipping...|]
    else do
      imageData <- runReq defaultHttpConfig $ requestImage year month
      swapCurrentWp year month imageData

mkYearMonth :: LocalTime -> (Year, Month)
mkYearMonth lt = do
  let fmtDate fmtStr = T.pack $ formatTime defaultTimeLocale fmtStr lt
      yearStr = fmtDate "%Y"
      monthStr = fmtDate "%B"
  (yearStr, monthStr)

getHomeDir :: (MonadIO m) => m String
getHomeDir = liftIO (homeDirectory <$> getUserEntryForName "cajun")

requestImage :: (MonadHttp m, MonadIO m) => Year -> Month -> m ByteString
requestImage year month = do
  let imageUrl =
        https "www.kriegs.net"
          /: "work"
          /: year
          /: "wallpapers"
          /: [i|kriegs_#{year}_#{month}_4K_3840x2160_calendar.jpg|]
  r <-
    req
      GET -- method
      imageUrl -- url
      NoReqBody -- no req body
      bsResponse -- bytestring
      mempty -- query params, headers, etc
  pure $ responseBody r

swapCurrentWp :: (MonadIO m) => Year -> Month -> ByteString -> m ()
swapCurrentWp year month imgData = do
  let fileName = [i|wallpaper-#{month}-#{year}.jpg|] :: ByteString
  homeDir <- getHomeDir
  let
    newFP = [i|#{homeDir}/Pictures/#{fileName}|] :: ByteString
    oldFP = [i|#{homeDir}/Pictures/wallpaper.jpg|] :: FilePath
  destExists <- liftIO $ fileExist newFP
  unless destExists do
    liftIO $ print ([i|rename #{oldFP} to #{newFP}|] :: String)
    liftIO $ rename (BS.pack oldFP) newFP -- wallpaper.jpg -> wallpaper-MONTH-YEAR.jpg
    liftIO $ print ([i|write #{BS.length imgData} bytes to #{oldFP}|] :: String)
    liftIO $ BS.writeFile oldFP imgData -- new image -> wallpaper.jpg

alreadyExists :: (MonadIO m) => Year -> Month -> m Bool
alreadyExists year month = do
  let relFileName = [i|wallpaper-#{month}-#{year}.jpg|] :: ByteString
  homeDir <- getHomeDir
  let newFP = [i|#{homeDir}/Pictures/#{relFileName}|] :: ByteString
  liftIO $ fileExist newFP
