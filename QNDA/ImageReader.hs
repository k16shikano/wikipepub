{-# LANGUAGE Arrows, FlexibleContexts #-}

module QNDA.ImageReader where
 
import Text.XML.HXT.Core hiding (xshow)

import System.Directory (copyFile, doesFileExist, getCurrentDirectory)
import qualified System.FilePath.Posix as FP
import qualified System.Process as Prc (readProcess)

import Network.HTTP.Base (urlEncode)
import qualified Control.Exception as E
import System.IO.Error 

-- import qualified Debug.Trace as DT (trace)

voidimg = "public/void.jpg"

imagePathToResourceName ::   FilePath      -- directory of image files
                          -> FilePath      -- input file name
                          -> IO [FilePath] -- list of image paths within the file
imagePathToResourceName imagesdir f = do
  links <- 
    runX (readDocument [ withIndent no
                       , withRemoveWS yes
                       , withValidate no
                       ] f
          >>>
          (listA $
           multi $ 
           hasName "img" >>> 
           (ifA (    hasAttrValue "class" (=="inlinemath") 
                 <+> hasAttrValue "class" (=="displaymath"))
            (none)
            (getAttrValue "src"))))

  paths <- mapM (
    \((basename, extension), counter) -> do
      let imgpathInEpub = imagesdir ++ (FP.takeBaseName f) ++ basename ++ (show counter) ++ extension
          imgsrc = imagesdir++basename++extension
      dfe <- doesFileExist imgsrc
      if dfe then (copyFile imgsrc imgpathInEpub) else (copyFile voidimg imgpathInEpub)
      return imgpathInEpub
    ) $ zip (map (\link -> (FP.takeBaseName link, FP.takeExtension link)) $ concat links) [1..]
  
  return paths

ignore :: E.SomeException -> IO ()
ignore _ = return ()

mkImgId :: FilePath  -> (FilePath, Int) -> String
mkImgId f (s,c) = FP.takeBaseName f ++ (urlEncode  $ FP.takeBaseName s) ++ show c ++ FP.takeExtension s

mkImgSrcPath :: String -> FilePath 
mkImgSrcPath imgid = FP.combine "images" $ imgid

imgElem :: (ArrowXml a) => FilePath -> a XmlTree XmlTree
imgElem f = 
   fromSLA 0 (
     processBottomUp (
        (((\(alt,path) -> 
            (eelem "img"
             += sattr "src" path 
             += sattr "alt" alt
             += sattr "class" "figure" 
             += (sattr "style" $< styleAttr)))
          $<
          (getAttrValue "src" &&& nextState (+1) >>> arr (mkImgId f) >>> (this &&& arr mkImgSrcPath)))
        )
        `when` 
        (hasName "img" >>> neg (hasAttrValue "class" (=="inlinemath") <+> hasAttrValue "class" (=="displaymath")))))

styleAttr :: (ArrowXml a) => a XmlTree String
styleAttr = 
  ((\(h,w) -> (case (h,w) of
                  ("","") -> addAttr "style" "width:90%;"
                  ("",w)  -> addAttr "style" $ "width:"++w++";"
                  (h,"")  -> addAttr "style" $ "height:"++h++";"
                  (h,w)   -> addAttr "style" $ "width:"++w++";height:"++h++";"))
   $<$ ((getAttrValue "height") &&& (getAttrValue "width")))
  >>> getAttrValue "style"

