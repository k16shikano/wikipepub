{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Web.Cookie
import Network.Wai.Middleware.Static
import Network.HTTP.Types
import System.Directory (removeDirectoryRecursive)
import System.IO.Temp

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad
import Control.Monad.IO.Class 
import qualified Control.Exception as E

import Text.XML.HXT.Core hiding ( xshow )

import Wpub.WikipediaToHTML
import QNDA.WriteEPUB

import qualified Debug.Trace as DT (trace)

main = do
  let port= 3000
  
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public")
    
    get "/wikipedia" $ do
      setHeader "Content-Type" "text/html"
      file "public/index.html"

    post "/wikipedia" $ do
      parameter <- param "titles"
      let titles = take 3 $  -- limit entries
                   map TL.strip $ TL.lines parameter
      epubid <- liftIO $ mkEpub titles
      
      setHeader "Content-Type" "text/html"
      setHeader "Set-Cookie" $ TL.pack "downloaded=yes"
      setHeader "Set-Cookie" $ TL.pack $ "epubid="++epubid
            
      file "public/index.html"


mkEpub titles = do
  withTempDirectory "public" "html." $ \htmldir -> do
    removeDirectoryRecursive htmldir `E.catch` ignore
    let epubid = drop 12 htmldir
    mapM ((wikiToHtml $ htmldir++"/") . TL.unpack) titles
    mkbookhtml htmldir titles
    writeEPUB (htmldir++"/") "book.html" $ "public/wikibook-"++epubid++".epub"
    return epubid

mkbookhtml htmldir titles =
  runX (replaceChildren 
        (eelem "book" 
         += (eelem "bookinfo"
             += (eelem "isbn" += (txt "none"))
             += (eelem "copyright" += (txt "Wikipedia"))
             += (eelem "authors" 
                 += (eelem "name"
                     += attr "role" (txt "author")
                     += (txt "Wikipedia")))
             += (eelem "booktitle"
                 += (txt "My Selected Wikipedia"))
            )
         += (eelem "frontmatter")
         += (eelem "mainmatter")
         += (catA (map (\t -> eelem "include" += txt (TL.unpack t)) titles)))
        >>>
        writeDocument [withOutputHTML] (htmldir++"/book.html"))
