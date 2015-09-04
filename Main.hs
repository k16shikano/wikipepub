{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Web.Cookie
import Network.Wai.Middleware.Static
import Network.HTTP.Types
import System.Directory (removeDirectoryRecursive)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Control.Monad
import Control.Monad.IO.Class 
import qualified Control.Exception as E

import Text.XML.HXT.Core hiding ( xshow )

import Wpub.WikipediaToHTML
import QNDA.WriteEPUB

main = do
  removeDirectoryRecursive htmldir `E.catch` ignore
  let port= 3000
  
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public")
    
    get "/wikipedia" $ do
      setHeader "Content-Type" "text/html"
      file "public/index.html"

    post "/wikipedia" $ do
      setHeader "Content-Type" "text/html"
      setHeader "Set-Cookie" $ TL.pack "downloaded=yes"
      
      parameter <- param "titles"
      let titles = take 3 $  -- limit entries
                   map TL.strip $ TL.lines parameter
      liftIO $ mkEpub titles
      file "public/index.html"

--      setHeader "Content-Type" "application/epub+zip"
--      file "wikibook.epub"


mkEpub titles = do
  mapM (wikiToHtml . TL.unpack) titles
  mkbookhtml titles
  writeEPUB htmldir "book.html" "public/wikibook.epub"

mkbookhtml titles =
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
        writeDocument [withOutputHTML] (htmldir++"book.html"))
  
