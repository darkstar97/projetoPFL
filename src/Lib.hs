module Lib
    ( 
        checaEstoque
    ) where

import Network.HTTP.Conduit
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL


checaEstoque :: IO ()
checaEstoque = do
    lbs <- simpleHttp "https://en.wikipedia.org/wiki/Main_Page"
    let lastModifiedDateTime = fromFooter $ parseTags lbs
    putStrLn $ "wikipedia was last modified on " 
        ++ CL.unpack lastModifiedDateTime
    where fromFooter = CL.unwords . drop 6 . CL.words
              . innerText . take 2 . dropWhile (~/= "<li id=footer-info-lastmod>")
