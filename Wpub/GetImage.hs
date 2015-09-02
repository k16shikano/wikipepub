{-# LANGUAGE OverloadedStrings #-}

module Wpub.GetImage where

import System.Directory ( createDirectoryIfMissing, doesFileExist )
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit ( simpleHttp, HttpException )

import Text.XML.HXT.Core hiding ( xshow, when )
import Text.XML.HXT.Curl

import System.IO (hPutStr, stderr)
import qualified Control.Exception as E
import System.IO.Error 
import Control.Monad
import Control.Monad.IO.Class

imagedir = "public/temp/images/"

getCommonsImage :: String -> String -> IO ()
getCommonsImage url name = do
  let workdir = imagedir
      filename = workdir++name
  createDirectoryIfMissing True workdir
  dlImageIfNotExist (url++"File:"++(urlEncode name)) filename
 
getImageURL :: String -> IO String
getImageURL path = do
  target <- runX (readDocument [withCurl [], withValidate no] path 
                  >>> deepest (hasAttrValue "class" (=="fullImageLink")
                               /> hasName "a" >>> getAttrValue "href"))
  return $ "https:"++(head target)

dlImageIfNotExist :: String -> String -> IO ()
dlImageIfNotExist path filename = 
  (do
      dfe <- (doesFileExist filename)
      when (not dfe) (getImageURL path >>= dlImage filename))
  `E.catch`
  (\e -> if isAlreadyExistsError e
         then hPutStr stderr ("Image " ++ filename ++" exists: using it.\n")
         else E.throwIO e)

dlImage :: String -> String -> IO ()
dlImage filename url = do
  (simpleHttp url >>= LB.writeFile filename)
  `E.catch`
  (\(E.SomeException e) -> hPutStr stderr ("Warning: Couldn't open " ++ filename ++ ": skipping.\n"))
