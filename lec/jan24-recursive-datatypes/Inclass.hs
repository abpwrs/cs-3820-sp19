module Inclass where

import TableTagsSol
import Tests

toRowElts :: [String] -> [TableHtml]
toRowElts [] = []
toRowElts (s : ss) = Element Td [ Raw s ] : toRowElts ss
  
rowToTable :: [TableHtml] -> TableHtml
rowToTable ss = Element Table [ Element Tr ss ]

toTable :: [String] -> TableHtml
toTable ss = rowToTable (toRowElts ss)

bigRow :: String -> [TableHtml]
bigRow s = Element Td [ Raw s ] : bigRow (s ++ " go")

repeated = let r = Element Tr (toRowElts ["so" , "long" , "good" , "bye"]) in
             Element Table (take 100 (repeat r))

bigTable :: TableHtml
bigTable = rowToTable (bigRow "go")
