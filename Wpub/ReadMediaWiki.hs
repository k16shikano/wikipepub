module Wpub.ReadMediaWiki where

import Data.String.Utils
import Data.List
import Data.Char (toUpper)

import Text.Parsec hiding (many, (<|>), Parser)
import Control.Applicative hiding (optional)
import Control.Monad.IO.Class

import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.XmlNode

import Network.HTTP.Base (urlEncode)

import Wpub.GetImage

data WST = WST {tempdir :: FilePath}
type WParser = ParsecT String WST IO
--type GenParser tok st = ParsecT [tok] st IO
--type CharParser st = GenParser Char st

wikiurl = "https://en.wikipedia.org/wiki/"
wst = WST {tempdir="temp/"}

mediaWikiToHtml :: FilePath -> String -> IO (Either ParseError [XmlTree])
mediaWikiToHtml dir s = runParserT parseMediaWiki wst{tempdir=dir++"/"} "" s

parseMediaWiki :: WParser [XmlTree]
parseMediaWiki =  manyTill block eof

block = choice [ try header
               , try indentation
               , try blist
               , try elist
               , try dlist
               , try blockquote
               , try source
               , try table
               , try para
               , try otherblock
               ]

header :: WParser XmlTree
header = do
  hc <- many1 $ string "=" 
  title <- many1 $ noneOf "="
  manyTill (string "=") (try newline)
  spaces
  let h = "h" ++ (show $ length hc)
  return $ mkelem h [] [mkText title]

para :: WParser XmlTree
para = do
  lookAhead $ noneOf "*#;:{|!"
  res <- many1 inline
  spaces
  return $ mkelem "p" [] res

blockquote :: WParser XmlTree
blockquote = do
  spaces
  string "<blockquote>"
  res <- manyTill para (try $ string "</blockquote>")
  spaces
  return $ mkelem "blockquote" [] res

source :: WParser XmlTree
source = do
  spaces
  string "<source" >> spaces >> manyTill (noneOf "/>") (try $ string ">")
  res <- manyTill (choice [try inline, try (mkText "\n" <$ newline)
                          , try ((mkText . (:[])) <$> noneOf "<&")])
         (try $ string "</source>")
  spaces
  return $ mkelem "pre" [mkattr "class" "source"] res

indentation :: WParser XmlTree
indentation = do
  many $ oneOf "*#"
  spaces
  depth <- string ":" 
  spaces  
  res <- manyTill inline (try newline)
  let depth = "margin-left:" ++ (show $ length depth) ++ "em;"
  return $ mkelem "div" [mkattr "style" "margin-left:1em;"] res

mklist kind whenSimple whenBlock = do
  lis <- many1 (choice $ concat [whenBlock, [try $ mkelem "li" [] <$> whenSimple]])
  spaces
  return $ mkelem kind [] lis

itemize :: WParser [XmlTree] -> WParser XmlTree -> WParser XmlTree
itemize main sub = do
  main' <- main
  subblist'  <- sub
  return $ mkelem "li" [] (main'++[subblist'])

item = itemize item' subblist
iitem = itemize iitem' subsubblist
iiitem = itemize iiitem' subsubsubblist

item' = (string "*" >> notFollowedBy (string ":")) >> spaces >> manyTill inline (try newline)
iitem' = (string "**" >> notFollowedBy (string ":")) >> spaces >> manyTill inline (try newline)
iiitem' = (string "***" >> notFollowedBy (string ":")) >> spaces >> manyTill inline (try newline)
iiiitem' = (string "****" >> notFollowedBy (string ":")) >> spaces >> manyTill inline (try newline)

blist = mklist "ul" item' [try indentation, try item]
subblist = mklist "ul" iitem' [try indentation, try iitem]
subsubblist = mklist "ul" iiitem' [try indentation, try iiitem]
subsubsubblist = mklist "ul" iiiitem' []

etem = itemize etem' subelist
eetem = itemize eetem' subsubelist
eeetem = itemize eeetem' subsubsubelist

etem' = (string "#" >> notFollowedBy (string ":")) >> spaces >> manyTill inline (try newline)
eetem' = (string "##" >> notFollowedBy (string ":")) >> spaces >> manyTill inline (try newline)
eeetem' = (string "###" >> notFollowedBy (string ":")) >> spaces >> manyTill inline (try newline)
eeeetem' = (string "####" >> notFollowedBy (string ":")) >> spaces >> manyTill inline (try newline)

elist = mklist "ol" etem' [try indentation, try etem]
subelist = mklist "ol" eetem' [try indentation, try eetem]
subsubelist = mklist "ol" eeetem' [try indentation, try eeetem]
subsubsubelist = mklist "ol" eeeetem' []

dlist = do
  spaces
  dt <- dtitle
  spaces
  dd <- option (mkText "") (choice [try ddesc, try blist, try elist, try indentation, try para, try table, try source])
  spaces
  return $ mkelem "dl" [] ([dt]++[dd])

dtitle = mkelem "dt" [] <$> (string ";" >> spaces >> manyTill inline (try newline))
ddesc = do  
  spaces
  depth <- string ":" 
  spaces 
  res <- manyTill inline (try newline)
  spaces
  return $ mkelem "dd" [] res

table = do
  spaces
  string "{|"
  spaces
  maybeattr <- option [] (try tableattribute)
  tb <- many1 tr
  spaces
  return $ mkelem "table" maybeattr tb
    
anAttr :: WParser XmlTree
anAttr = do
  n <- attrname
  spaces
  string "="
  spaces
  v <- attrvalue
  return $ mkattr n v

attrname = manyTill alphaNum (lookAhead $ try $ oneOf "\n= ")
attrvalue = choice [ try (char '"' *> manyTill (noneOf "\"\n|") (try (char '"')))
                   , many1 (noneOf " \n")]

tableattribute :: WParser [XmlTree]
tableattribute = do
  spaces
  attrs <- sepEndBy1 anAttr (many (oneOf " \n"))
  option [] (try (string "|-"))
  spaces
  return $ attrs

cellattribute :: WParser [XmlTree]
cellattribute = do
  spaces
  attrs <- sepEndBy1 anAttr (oneOf "\n ")
  spaces >> (string "|" >> notFollowedBy (oneOf "|+}"))
  spaces
  return $ attrs

tr :: WParser XmlTree
tr = mkelem "tr" [] <$> manyTill tcell (choice [try $ string "|-", try $ string "|}"]) <* spaces

tcell = do  
  spaces
  mark <- oneOf "|!"
  let tx = if mark == '|' then "td" else "th"
  maybeattr <- option [] (try cellattribute)
  spaces
  cell <- manyTill (choice [try (mkelem "div" [] <$> many1 inline), try blist, try elist, try otherblock ])
              (choice [ try (spaces >> lookAhead (try $ string "|-"))
                      , try (spaces >> lookAhead (try $ string "|}"))
                      , try (string "|" >> spaces >> lookAhead (try $ string "|"))
                      , try (string "!" >> spaces >> lookAhead (try $ string "!"))
                      , try (newline >> spaces >> lookAhead (try $ string "|"))
                      , try (newline >> spaces >> lookAhead (try $ string "!"))
                      ])
  spaces
  return $ mkelem tx maybeattr cell

otherblock :: WParser XmlTree
otherblock = do 
  spaces
  many1 (noneOf "\n\r")
  spaces
  return $ mkText "\n\n"
  
inline :: WParser XmlTree
inline = choice [ mkText "" <$ try ((string "<!--") >> manyTill anyChar (try $ string "-->"))
                , mkText "    " <$ try (char '\t')
                , mkEntityRef "amp" <$ try (string "&")
                , mkEntityRef "nbsp" <$ try (string "&nbsp;")
                , mkEntityRef "ndash" <$ try (string "&ndash;")
                , mkEntityRef "mdash" <$ try (string "&mdash;")
                , mkelem "br" [] [] <$ try (string "<br/>")
                , mkText "!" <$ try ((char '!') >> notFollowedBy (char '!'))
                , try emph
                , try bold
                , try italic
                , try underline
                , try code, try tt, try small
                , try sub, try sup
                , try math                  
                , mkText "'" <$ try (char '\'')
                , try citeweb, try citeweb'
                , try citebook, try citebook'
                , try citenews, try citenews'
                , try ref
                , try refref
                , try convert
                , try ipac
                , try respell
                , try otherExt
                , try image
                , try link
                , try bracket
                , try curlybracket
                , try inlineText
                ]

inlineText :: WParser XmlTree
inlineText = mkText <$> many1 inlineChar

inlineChar :: WParser Char
inlineChar = satisfy $ \c ->
    not (c == '['  || c == ']' || c == '|' || c == '\'' || c == '<' ||
         c == '{'  || c == '}' || c == '!' || c == '&'  ||
         c == '\t' || c == '\n')

emph :: WParser XmlTree
emph = do
  string "'''''"
  t <- manyTill inline (try $ string "'''''")
  return $ mkelem "emph" [] t

bold :: WParser XmlTree
bold = do
  string "'''"
  t <- manyTill inline (try $ string "'''")
  return $ mkelem "b" [] t

italic :: WParser XmlTree
italic = do
  string "''"
  t <- manyTill inline (try $ string "''")
  return $ mkelem "i" [] t

underline = htmltag "u"
code = htmltag "code"
tt = htmltag "tt"
small = htmltag "small"
math = htmltag "span"
sub = htmltag "sub"
sup = htmltag "sup"

htmltag :: String -> WParser XmlTree
htmltag s = do
  string ("<"++s) >> spaces >> manyTill (noneOf "/>") (string ">")
  t <- manyTill (choice [try inline, try (mkText "\n" <$ newline)
                        , try ((mkText . (:[])) <$> noneOf "<&")])
       (try $ string ("</"++s++">"))  
  return $ mkelem s [] t

citation :: ParsecT String WST IO a -> ParsecT String WST IO a -> ([[XmlTree]] -> XmlTree) -> WParser XmlTree
citation start end f = do
  refstart
  start >> spaces
  string "|" >> spaces
  ts <- sepBy1 (many1 inline) $ try (string "|")
  end
  refend
  return $ f ts

refstart = string "<ref" >> spaces >> manyTill inlineChar (try $ string ">") >> spaces
refend   = string "</ref>"
citeweb  = citation (string "{{cite web") (string "}}") (\ts -> (mkelem "sup" [] [mkText "cite"]))
citeweb' = citation (string "{{Cite web") (string "}}") (\ts -> (mkelem "sup" [] [mkText "cite"]))
citenews  = citation (string "{{cite news") (string "}}") (\ts -> (mkelem "sup" [] [mkText "cite"]))
citenews' = citation (string "{{Cite news") (string "}}") (\ts -> (mkelem "sup" [] [mkText "cite"]))
citebook  = citation (string "{{cite book") (string "}}") (\ts -> (mkelem "sup" [] [mkText "cite"]))
citebook' = citation (string "{{Cite book") (string "}}") (\ts -> (mkelem "sup" [] [mkText "cite"]))

ref :: WParser XmlTree
ref = mkText "" <$ (string "<ref" >> spaces >> manyTill inlineChar (try $ string "/>") <* (skipMany (string " ")))

refref :: WParser XmlTree
refref = do
  string "<ref" >> spaces >> manyTill inlineChar (try (string ">"))>> spaces
  manyTill inline (try (string "</ref>"))  <* (skipMany (string " "))
  return $ mkText ""

convert :: WParser XmlTree
convert = do
  string "{{convert" 
  spaces
  string "|"
  (v:vs) <- sepBy1 (many1 inline) $ try (string "|")
  string "}}"
  return $ mkText (concatText v ++ concatText (head vs))

ipac :: WParser XmlTree
ipac = do
  string "{{"
  choice [string "IPAc-en"]
  spaces
  string "|"
  vs <- sepBy1 (many1 inline) $ try (string "|")
  string "}}"
  return $ mkText $ concat (map concatText vs)

respell :: WParser XmlTree
respell = do
  string "{{Respell"
  spaces
  string "|"
  vs <- sepBy1 (many1 inline) $ try (string "|")
  string "}}"
  return $ mkText $ concat (map concatText vs)

otherExt :: WParser XmlTree
otherExt = do
  string "{{" 
  spaces
  s <- sepBy1 (many1 inline) $ try (string "|")
  string "}}"
  return $ mkText ""

image :: WParser XmlTree
image = do
  isInline <- choice [try (False <$ string "[[File:")
                     ,try (True  <$ string "[[Image:")
                     ]
  s <- sepBy1 (many1 inline) $ try (string "|")
  string "]]"
  
  let options = map concatText $ tail s 
      isFormat = (`elem` ["border", "frameless", "frame", "thumb", "thumbnail"])
      isResizing str = (str=="upright") || "px" `isSuffixOf` str
      format  = filter isFormat options
      resizing = filter isResizing options
      rest = dropWhile (\c -> (isFormat c) || (isResizing c)) options
      caption = if rest==[] then "" else head rest
  
  let name' = join "_" $ words $ concatText $ head s
      thumb = "thumb" `elem` format || "thumbnail" `elem` format
      widthattr = case resizing of
        [] -> if thumb then mkattr "width" "222px" else mkattr "" []
        ("upright":_) -> if thumb then mkattr "width" "222px" else mkattr "" []
        (px:_) -> mkattr "width" px
      name = (toUpper $ head name'):(tail name')

  wst <- getState
  liftIO $ getCommonsImage (tempdir wst) wikiurl name
  
  let img = mkelem "img" [ mkattr "src" ("images/"++name)
                         , widthattr ] []

  return $ if isInline 
           then img 
           else mkelem "figure" [] [img, mkelem "figcaption" [] [(mkText caption)]]
 
concatText :: [XmlTree] -> String
concatText xs = concat $ 
                map (\x -> case getText x of
                        Just str -> str
                        Nothing -> "")
                xs

link :: WParser XmlTree
link = do
  string "[["
  (t:ts) <- sepBy1 (many1 inline) $ try (string "|")
  string "]]"
  let href = mkattr "href" $ concatText $ if ts==[] then t else (head ts)
  return $ mkelem "a" [href] t

bracket :: WParser XmlTree
bracket = do
  string "["
  res <- manyTill inline (try $ string "]")
  return $ mkelem "span" [] res 

curlybracket :: WParser XmlTree
curlybracket = do
  string "{" 
  res <- manyTill inline (try $ string "}")
  return $ mkelem "span" [] $ [mkText "{"]++res++[mkText "}"] 

mkelem :: String -> XmlTrees -> XmlTrees -> XmlTree
mkelem name attrs elems = mkElement (mkName name) attrs elems

mkattr :: String -> String -> XmlTree
mkattr k v = mkAttr (mkName k) [mkText v]
