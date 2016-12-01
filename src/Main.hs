--Comics save to the comics folder (duh)

module Main where

import Network.Connection
import Network.HTTP.Conduit

import Text.XML.HXT.Core
import Text.HandsomeSoup

import qualified Data.ByteString.Lazy.Char8 as BL

smbcComic :: String
smbcComic = "http://www.smbc-comics.com/comic/"

main :: IO ()
main = do
  body <- simpleHttp "http://www.smbc-comics.com/comic/archive"
  let doc = readString [withParseHTML yes, withWarnings no](BL.unpack body)
  comics <- runX $ doc //> css "select" >>> hasAttrValue "name" (=="comic")
                       >>> css "option" >>> getAttrValue "value"
                       >>. filter (not . null)
  mapM_ picBytes $ take 100 comics

picSource :: String -> IO (String)
picSource comicUrl = do
  body <- simpleHttp $ smbcComic ++ comicUrl
  let doc = readString [withParseHTML yes, withWarnings no](BL.unpack body)
  images <- runX $ doc //> css "img" >>> hasAttrValue "id" (=="cc-comic")
                       >>> getAttrValue "src"
  return $ head images

picBytes :: String -> IO ()
picBytes image = do
  body <- simpleHttp =<< picSource image
  putStrLn $ "downloading " ++ image ++ " :D"
  BL.writeFile ("comics/" ++ image ++ ".gif") body
