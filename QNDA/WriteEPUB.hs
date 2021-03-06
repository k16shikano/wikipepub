﻿{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module QNDA.WriteEPUB where

import Text.XML.HXT.Core hiding (xshow)
import Text.XML.HXT.Arrow.XmlArrow hiding (xshow)
import Text.XML.HXT.DOM.ShowXml (xshow)

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Map as Map
import Data.Hashable (hash)

import Control.Monad as M hiding (when)

import System.Environment (getArgs)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Control.Exception as E
import System.IO.Error 

import qualified Codec.Archive.Zip as ZIP

import QNDA.HtmlReader (readHtml, getTextFromNode)
import QNDA.MathReader (mathElemToResourceName, genImageFromEqString, hasMathElem)
import QNDA.ImageReader (imagePathToResourceName)
import QNDA.MetaInformation (mkCoverpage, genOkuzuke, getMetaInfo, mkOpf, mkContainer, getIsbn)
import QNDA.Counter (readLabelInfo)
import QNDA.Toc (mkTocpage, mkNcx, mkHeaderCnt)

import qualified Debug.Trace as DT (trace)

-- main
writeEPUB :: String -> String -> String -> IO ()
writeEPUB htmldir main out = do
  epochtime <- floor `fmap` getPOSIXTime  
  let mkEntry path content = ZIP.toEntry path epochtime content
  let cssfiles = ["public/css/epub.css"
                 ]
  let cssdir = "public/css"
  let coverimg = "public/cover.jpg"
  let coverfilename = "public/titlepage.xhtml"
  let tocpagefile = "toc.xhtml"
  let okuzukepagefile = htmldir++"okuzuke.xhtml"
  let ncxfile = "toc.ncx"
  let auxfile = "book.aux" -- if you will
  let outputFileName = out
  let book = htmldir++main
  
  isbn <- getIsbn book
    
  coverfile <- mkCoverpage coverimg
  coverimgEntry <- ZIP.readEntry [] coverimg
  let coverfileEntry = mkEntry coverfilename $ fromString . xshow $ coverfile
  
  okuzuke <- genOkuzuke book
  let okuzukeEntry = mkEntry okuzukepagefile $ fromString . xshow $ okuzuke
  
  -- Qualifing each file with an information whether it's to be a chapter, frontmatter, or appendix.
  -- The information must resist in the "book" file.  
  htmls <- runX (
    readDocument [withValidate no] book
    >>>
    fromSLA (0,"frontmatter", "")
    (multi (ifA (hasName "include")
            (nextState . (\(filename, numOfh1) (n,x,_) -> (n+1, x, filename))
                           $< ((getChildren >>> getText >>> arr ((htmldir++) . (++".xhtml")))
                               &&& (listA (multi (hasName "h1")) >>> arr length)))
            (ifA (hasName "mainmatter")
             (constA (0, "chapter", "") >>> setState)
             (ifA (hasName "appendix") 
              (constA (0, "appendix", "") >>> setState) 
              (none)))))
    )
  
  let htmlFiles = filter (\(n,t,f) -> f/="") htmls
  let htmlFilenames = map (\(n,t,f) -> f) htmlFiles
      
  -- Retrieving reference-ids from all the html files and letting each id be pair with the filename,
  -- in order to make distinct ids in the epub file.  
  labelsAndFiles <-
    mapM (\s -> do 
             links <- runX (readDocument [withIndent no
                                         ,withRemoveWS yes
                                         ,withValidate no
                                         ] s
                            >>>
                            multi (ifA (hasName "ref" <+> hasName "pref" <+> hasName "pageref")
                                   (constA "")
                                   (getAttrValue "label" <+> getAttrValue "name")))
             return $ map (flip (,) s) $ filter (/="") links
         ) htmlFilenames
  let internalLinkLabels = Map.fromList $ concat labelsAndFiles

  maths <- mapM mathElemToResourceName htmlFilenames
  mapM_ (\(imagepath, equation) -> genImageFromEqString imagepath equation) $ concat maths
  let mathSnipets = Map.fromList $ concat maths
  let mathimages = map fst $ concat maths
  mathImageEntries <- mapM (ZIP.readEntry []) mathimages
  hasmath <- mapM hasMathElem htmlFilenames
  let htmlsWithSVG = concat hasmath

  labelmap <- readLabelInfo htmlFiles
  htmlData <- mapM (\(n,t,f) -> readHtml f internalLinkLabels mathSnipets labelmap n t) htmlFiles
  let htmlEntries = zipWith (\(n,t,f) d -> mkEntry f d) htmlFiles htmlData

  images <- liftM concat $ mapM (imagePathToResourceName (htmldir++"images/")) htmlFilenames
  imageEntries <- mapM (ZIP.readEntry []) images
  
  metadata <- getMetaInfo book
  opf <- mkOpf metadata htmlFiles htmlsWithSVG images mathimages (coverimg, coverfilename) tocpagefile okuzukepagefile ncxfile cssfiles
  let opfEntry = mkEntry "content.opf" $ fromString . xshow $ opf

  putStrLn $ show htmlFiles
  
  -- extract all the header elements to generate toc and ncx
  headers <- 
    mapM (\(n,t,f) -> do
             runX (readDocument [withValidate no] f 
                   >>> 
                   (multi
                    (ifA (hasName "h1" <+> hasName "h2" <+> hasName "appendix")
                     (this) (none))
                    >>>
                    ((getName) &&& (getTextFromNode labelmap) &&& constA f &&& 
                     (ifA (hasAttrValue "nonum" (=="yes")) (constA $ mkHeaderCnt "other" n) (constA $ mkHeaderCnt t n)))))
         ) htmlFiles 

  ncx <- mkNcx (concat isbn) (concat headers)
  let ncxEntry = mkEntry ncxfile $ fromString . xshow $ ncx

  tocpage <- mkTocpage $ concat headers
  let tocpageEntry = mkEntry tocpagefile $ fromString . xshow $ tocpage
  
  let mimetypeEntry = mkEntry "mimetype" $ fromString "application/epub+zip"
  
  stylesheetEntry <- ZIP.readEntry [ZIP.OptRecursive] cssdir
      
  container <- mkContainer
  let containerEntry = mkEntry "META-INF/container.xml" $ fromString . xshow $ container
      
  -- gather all the entries around!
  let archive' =  foldr ZIP.addEntryToArchive ZIP.emptyArchive (containerEntry : ncxEntry : opfEntry : stylesheetEntry :
                                                                coverfileEntry : coverimgEntry : tocpageEntry : okuzukeEntry : 
                                                                (htmlEntries ++ imageEntries ++ mathImageEntries))

  archive'' <- ZIP.addFilesToArchive [ZIP.OptRecursive] archive' [ cssdir ]
  
  -- ensure mimetype to be the first entry
  let archive = ZIP.addEntryToArchive mimetypeEntry archive''
      
  let a = ZIP.fromArchive archive
 
  B.writeFile outputFileName a 
  
  return ()
  
ignore :: E.SomeException -> IO ()
ignore _ = return ()

