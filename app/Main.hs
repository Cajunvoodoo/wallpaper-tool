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
  , rename
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

main :: IO ()
main = do
  (homeDirectory -> homeDir) <- getArgs >>= firstArg >>= getUserEntryForName
  tz <- getCurrentTimeZone
  (year, month) <- mkYearMonth . utcToLocalTime tz <$> getCurrentTime
  newImgExists <- alreadyExists year month homeDir
  if newImgExists
    then
      print
        ( [i|Warning: kriegs_#{year}_#{month}_4K_3840x2160_calendar.jpg already exists, skipping...|]
            :: T.Text
        )
    else do
      imageData <- runReq defaultHttpConfig $ requestImage year month
      swapCurrentWp year month homeDir imageData

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

swapCurrentWp :: (MonadIO m) => Year -> Month -> HomeDir -> ByteString -> m ()
swapCurrentWp year month homeDir imgData = do
  let fileName = [i|wallpaper-#{month}-#{year}.jpg|] :: ByteString
  let
    newFP = [i|#{homeDir}/Pictures/#{fileName}|] :: ByteString
    oldFP = [i|#{homeDir}/Pictures/wallpaper.jpg|] :: FilePath
  destExists <- liftIO $ fileExist newFP
  unless destExists do
    liftIO $ print ([i|rename #{oldFP} to #{newFP}|] :: T.Text)
    liftIO $ rename (BS.pack oldFP) newFP -- wallpaper.jpg -> wallpaper-MONTH-YEAR.jpg
    liftIO $ print ([i|write #{BS.length imgData} bytes to #{oldFP}|] :: T.Text)
    liftIO $ BS.writeFile oldFP imgData -- new image -> wallpaper.jpg

alreadyExists :: (MonadIO m) => Year -> Month -> HomeDir -> m Bool
alreadyExists year month homeDir = do
  let relFileName = [i|wallpaper-#{month}-#{year}.jpg|] :: ByteString
  let newFP = [i|#{homeDir}/Pictures/#{relFileName}|] :: ByteString
  liftIO $ fileExist newFP
