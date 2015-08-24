{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as BA
import qualified Text.Blaze.Html.Renderer.Text as BR
import Network.Wai.Middleware.Static

import qualified Data.Text.Lazy as TL
import Control.Monad
import System.Environment
import Control.Monad.IO.Class
import Text.XML.HXT.Core hiding ( xshow )

import Wpub.WikipediaToHTML

blaze = html . BR.renderHtml

main = do
  port <- liftM read $ getEnv "PORT"     
  scotty port $ do    
    middleware $ staticPolicy (noDots >-> addBase "public")

    get "/wikipedia" $ do
      blaze $ do
        B.html $ do
          B.body $ do
            B.h1 "Read Wikipedia with EPUB" 
            B.form B.! BA.method "post" B.! BA.action "/wikipedia" $ do
              (B.textarea $ "") B.! BA.name "titles" B.! BA.rows "3"
              (B.button $ "Submit") B.! BA.type_ "submit"

    post "/wikipedia" $ do
      parameter <- param "titles"
      let titles = map TL.strip $ TL.lines parameter
      liftIO $ mapM (wikiToHtml . TL.unpack) titles
      liftIO $ mkbookhtml titles
    
      let pagenames = map (("temp/" `TL.append` ) . (`TL.append` ".html")) (titles)
      redirect $ head pagenames

mkbookhtml titles =
  runX (replaceChildren 
        (eelem "book" 
         += (eelem "bookinfo"
             += (eelem "authors" 
                 += (eelem "name"
                     += attr "role" (txt "author")
                     += (txt "Wikipedia")))
             += (eelem "booktitle"
                 += (txt "My Selected Wikipedia"))
            )
         += (eelem "fromtmatter")
         += (catA (map (\t -> eelem "include" += txt (TL.unpack t)) titles)))
        >>>
        writeDocument [withOutputHTML] "temp/book.html")
  
