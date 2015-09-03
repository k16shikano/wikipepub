{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Text.Blaze.Html.Renderer.Text as BR
import qualified Text.Blaze.Internal as BI ( string )
import Network.Wai.Middleware.Static

import qualified Data.Text.Lazy as TL
import Control.Monad
import System.Environment
import System.IO
import Control.Monad.IO.Class
import Text.XML.HXT.Core hiding ( xshow )

import Wpub.WikipediaToHTML
import QNDA.WriteEPUB

blaze = html . BR.renderHtml

main = do
--  removeDirectoryRecursive htmldir `E.catch` ignore
  let port= 3000
  --  port <- liftM read $ getEnv "PORT" 
  
--  h <- openFile "public/js/waitmessage.js" ReadMode
--  jstext <- hGetContents h
  
  scotty port $ do
    middleware $ staticPolicy (noDots >-> addBase "public")

    get "/wikipedia" $ do
      blaze $ do
        B.html $ do
          B.head $ do
            B.title "Read Wikipedia with EPUB"
            (B.script $ "") B.! BA.type_ "text/javascript" B.! BA.src "http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js"
            (B.script $ "") B.! BA.type_ "text/javascript" B.! BA.src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.9.2/jquery-ui.min.js"
            (B.script $ "") B.! BA.type_ "text/javascript" B.! BA.src "http://malsup.github.io/jquery.blockUI.js"
            (B.script $ "") B.! BA.type_ "text/javascript" B.! BA.src "js/waitmessage.js"
          B.body $ do
            B.h1 "Read Wikipedia with EPUB" 
            B.form B.! BA.method "post" B.! BA.action "/wikibook" $ do
              (B.textarea $ "") B.! BA.name "titles" B.! BA.rows "5"
              (B.br)
              (B.button $ "Submit") B.! BA.type_ "submit" B.! BA.id "waitmessage"

    get "/wikibook" $ do
      redirect "/wikipedia"
      
    post "/wikibook" $ do
      parameter <- param "titles"
      let titles = take 3 $  -- limit entries
                   map TL.strip $ TL.lines parameter
      liftIO $ mkEpub titles
      
      setHeader "Content-Type" "application/epub+zip"
      file "wikibook.epub"


mkEpub titles = do
  mapM (wikiToHtml . TL.unpack) titles
  mkbookhtml titles
  writeEPUB htmldir "book.html" "wikibook.epub"

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
  
