module Wpub.ReadMediaWiki where

import Data.String.Utils
import Data.List

import Text.Parsec hiding (many, (<|>), Parser)
import Control.Applicative hiding (optional)
import Control.Monad.IO.Class

import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.XmlNode

import Network.HTTP.Base (urlEncode)

import Wpub.GetImage

type Parser = ParsecT String () IO
type GenParser tok st = ParsecT [tok] st IO
type CharParser st = GenParser Char st

wikiurl = "https://en.wikipedia.org/wiki/"

mediaWikiToHtml :: String -> IO (Either ParseError [XmlTree])
mediaWikiToHtml s = runParserT parseMediaWiki () "" s

parseMediaWiki :: Parser [XmlTree]
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

header :: Parser XmlTree
header = do
  hc <- many1 $ string "=" 
  title <- many1 $ noneOf "="
  manyTill (string "=") (try newline)
  spaces
  let h = "h" ++ (show $ length hc)
  return $ mkelem h [] [mkText title]

para :: Parser XmlTree
para = do
  lookAhead $ noneOf "*#;:{|!"
  res <- many1 inline
  spaces
  return $ mkelem "p" [] res

blockquote :: Parser XmlTree
blockquote = do
  spaces
  string "<blockquote>"
  res <- manyTill para (try $ string "</blockquote>")
  spaces
  return $ mkelem "blockquote" [] res

source :: Parser XmlTree
source = do
  spaces
  string "<source" >> spaces >> manyTill (noneOf "/>") (try $ string ">")
  res <- manyTill (choice [try inline, try (mkText "\n" <$ newline)
                          , try ((mkText . (:[])) <$> noneOf "<&")])
         (try $ string "</source>")
  spaces
  return $ mkelem "pre" [mkattr "class" "source"] res

indentation :: Parser XmlTree
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

itemize :: Parser [XmlTree] -> Parser XmlTree -> Parser XmlTree
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
    
anAttr :: Parser XmlTree
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

tableattribute :: Parser [XmlTree]
tableattribute = do
  spaces
  attrs <- sepEndBy1 anAttr (many (oneOf " \n"))
  option [] (try (string "|-"))
  spaces
  return $ attrs

cellattribute :: Parser [XmlTree]
cellattribute = do
  spaces
  attrs <- sepEndBy1 anAttr (oneOf "\n ")
  spaces >> (string "|" >> notFollowedBy (oneOf "|+}"))
  spaces
  return $ attrs

tr :: Parser XmlTree
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

otherblock :: Parser XmlTree
otherblock = do 
  spaces
  many1 (noneOf "\n\r")
  spaces
  return $ mkText "\n\n"
  
inline :: Parser XmlTree
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

inlineText :: Parser XmlTree
inlineText = mkText <$> many1 inlineChar

inlineChar :: Parser Char
inlineChar = satisfy $ \c ->
    not (c == '['  || c == ']' || c == '|' || c == '\'' || c == '<' ||
         c == '{'  || c == '}' || c == '!' || c == '&'  ||
         c == '\t' || c == '\n')

emph :: Parser XmlTree
emph = do
  string "'''''"
  t <- manyTill inline (try $ string "'''''")
  return $ mkelem "emph" [] t

bold :: Parser XmlTree
bold = do
  string "'''"
  t <- manyTill inline (try $ string "'''")
  return $ mkelem "b" [] t

italic :: Parser XmlTree
italic = do
  string "''"
  t <- manyTill inline (try $ string "''")
  return $ mkelem "i" [] t

underline = htmltag "u"
code = htmltag "code"
tt = htmltag "tt"
small = htmltag "small"
math = htmltag "math"
sub = htmltag "sub"
sup = htmltag "sup"

htmltag :: String -> Parser XmlTree
htmltag s = do
  string ("<"++s) >> spaces >> manyTill (noneOf "/>") (string ">")
  t <- manyTill (choice [try inline, try (mkText "\n" <$ newline)
                        , try ((mkText . (:[])) <$> noneOf "<&")])
       (try $ string ("</"++s++">"))  
  return $ mkelem s [] t

citation :: ParsecT String () IO a -> ParsecT String () IO a -> ([[XmlTree]] -> XmlTree) -> Parser XmlTree
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

ref :: Parser XmlTree
ref = mkText "" <$ (string "<ref" >> spaces >> manyTill inlineChar (try $ string "/>") <* (skipMany (string " ")))

refref :: Parser XmlTree
refref = do
  string "<ref" >> spaces >> manyTill inlineChar (try (string ">"))>> spaces
  manyTill inline (try (string "</ref>"))  <* (skipMany (string " "))
  return $ mkText ""

convert :: Parser XmlTree
convert = do
  string "{{convert" 
  spaces
  string "|"
  (v:vs) <- sepBy1 (many1 inline) $ try (string "|")
  string "}}"
  return $ mkText (concatText v ++ concatText (head vs))

ipac :: Parser XmlTree
ipac = do
  string "{{"
  choice [string "IPAc-en"]
  spaces
  string "|"
  vs <- sepBy1 (many1 inline) $ try (string "|")
  string "}}"
  return $ mkText $ concat (map concatText vs)

respell :: Parser XmlTree
respell = do
  string "{{Respell"
  spaces
  string "|"
  vs <- sepBy1 (many1 inline) $ try (string "|")
  string "}}"
  return $ mkText $ concat (map concatText vs)

otherExt :: Parser XmlTree
otherExt = do
  string "{{" 
  spaces
  s <- sepBy1 (many1 inline) $ try (string "|")
  string "}}"
  return $ mkText ""

image :: Parser XmlTree
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
  
  let name = ("File:"++) $ urlEncode $ join "_" $ words $ concatText $ head s
      thumb = "thumb" `elem` format || "thumbnail" `elem` format
      widthattr = case resizing of
        [] -> if thumb then mkattr "width" "222px" else mkattr "" []
        ("upright":_) -> if thumb then mkattr "width" "222px" else mkattr "" []
        (px:_) -> mkattr "width" px 

  liftIO $ getCommonsImage wikiurl name
  
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

link :: Parser XmlTree
link = do
  string "[["
  (t:ts) <- sepBy1 (many1 inline) $ try (string "|")
  string "]]"
  let href = mkattr "href" $ concatText $ if ts==[] then t else (head ts)
  return $ mkelem "a" [href] t

bracket :: Parser XmlTree
bracket = do
  string "["
  res <- manyTill inline (try $ string "]")
  return $ mkelem "span" [] res 

curlybracket :: Parser XmlTree
curlybracket = do
  string "{" 
  res <- manyTill inline (try $ string "}")
  return $ mkelem "span" [] $ [mkText "{"]++res++[mkText "}"] 

mkelem :: String -> XmlTrees -> XmlTrees -> XmlTree
mkelem name attrs elems = mkElement (mkName name) attrs elems

mkattr :: String -> String -> XmlTree
mkattr k v = mkAttr (mkName k) [mkText v]
