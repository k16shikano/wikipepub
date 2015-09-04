{-# LANGUAGE OverloadedStrings #-}

module Wpub.GetImage where

import System.Directory ( createDirectoryIfMissing, doesFileExist )
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit ( simpleHttp, HttpException )

import Text.XML.HXT.Core hiding ( xshow, when )
import Text.XML.HXT.Curl

import System.IO (hPutStr, stderr)
import qualified System.FilePath.Posix as FP
import qualified Control.Exception as E
import System.IO.Error 
import Control.Monad
import Control.Monad.IO.Class

import qualified Debug.Trace as DT (trace)

imagedir = "public/temp/images/"

getCommonsImage :: String -> String -> IO ()
getCommonsImage url name = do
  let workdir = imagedir
      filename = workdir++name
  createDirectoryIfMissing True workdir
  dlImageIfNotExist (url++"File:"++(urlEncode name)) filename
 
getImageURL :: String -> IO String
getImageURL path = do
  nandw <- runX (readDocument [withCurl [], withValidate no] (DT.trace path path) 
                 >>> deepest (hasAttrValue "class" (=="fullImageLink")
                              /> hasName "a"
                              >>> (getAttrValue "href" &&& 
                                   (this /> hasName "img" >>> getAttrValue "data-file-width"))))
  let original = if [] == nandw then "" else fst $ head nandw
      width    = if [] == nandw then "" else snd $ head nandw
  
  let fn  = FP.takeFileName original
      commons = take 4 $ FP.splitPath original
      rest    = drop 4 $ FP.splitPath original
      thumb   = commons ++ ["thumb/"] ++ rest 
      resized = (join thumb) ++ "/300px-" ++ fn      
      extns   = FP.takeExtension original
      target  = if extns == ".svg" then original
                else if (read width :: Int) <= 300 then original 
                     else resized
  
  return $ "https:"++target

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
