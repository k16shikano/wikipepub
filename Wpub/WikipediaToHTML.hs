{-# LANGUAGE Arrows, FlexibleContexts #-}

module Wpub.WikipediaToHTML (wikiToHtml) where

import Text.XML.HXT.Core hiding ( xshow )
import Text.XML.HXT.Curl
import Text.XML.HXT.DOM.QualifiedName ( mkName )
import qualified Text.XML.HXT.DOM.XmlNode as X ( mkText )

import System.Directory ( createDirectoryIfMissing, removeDirectoryRecursive )
import Data.String.Utils

import qualified Control.Exception as E
import System.IO.Error 

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 ( fromString )

import Wpub.ReadMediaWiki
import Wpub.GetImage

-- htmldir = "public/temp/"

wikiToHtml :: FilePath -> String -> IO ()
wikiToHtml htmldir s = do
  let url = "https://en.wikipedia.org/w/index.php?title=Special:Export&pages="++s++"&offset=1&limit=1&action=submit"
  
  createDirectoryIfMissing True (htmldir++"/images")
  putStrLn url
  runX (
    readDocument [ withIndent yes
                 , withRemoveWS no
                 , withValidate no
                 , withCurl []
                 ] url
    >>>
    processChildren (
      eelem "body"
      += getTitle
{-      += (deepest (hasName "text" >>> getChildren))) >>> writeDocument [] ("page.html")-}
      += (deepest ( hasName "text" />
                    getText        >>>
                    arrIO (mediaWikiToHtml htmldir) >>>
                    arrL (\r -> case r of
                            Left err -> []
                            Right x -> x))))
    >>>
    writeDocument [] (htmldir++"/"++s++".xhtml")
    )
  return ()

getTitle = multi (hasName "page" />
                  hasName "title" >>> setQName (mkName "h1"))

ignore :: E.SomeException -> IO ()
ignore _ = return ()
