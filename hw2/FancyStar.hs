module FancyStar where

import SvgStar

drawFancyLine :: Int -> Int -> Int -> Int -> String -> String
drawFancyLine x1 y1 x2 y2 color = "<line x1 =\"" ++ show x1 ++ "\" y1 = \"" ++ show y1 ++ "\" x2 = \"" ++ show x2 ++ "\" y2 = \"" ++ show y2 ++ "\" style=\"fill:none;stroke:" ++ color ++ "\"/>"

drawFancyList :: [Edge] -> String -> String
drawFancyList [] _ = ""
drawFancyList (((x1, y1),(x2, y2)):xs) color = (drawFancyLine x1 y1 x2 y2 color) ++ "\n" ++ drawFancyList xs color

showFancyStar :: Int -> Int -> Int -> Int -> Int -> String -> String
showFancyStar tx ty r sep n color = drawFancyList (edgesFromPoints sep 0 (translateAll tx ty (evenlySpacedPoints n n r))) color

writeFancyStar :: String -> Int -> Int -> Int -> Int -> Int -> String -> IO ()
writeFancyStar filename tx ty r sep n color =
  writeFile filename (htmlStart ++ showFancyStar tx ty r sep n color ++ htmlEnd)

writeFancyExample :: String -> Int -> Int -> String -> IO ()
writeFancyExample file = writeFancyStar (file ++ ".html") 300 300 200 

main :: IO ()
main = do
    writeFancyExample "fancyStar1" 2 6 "red"
    writeFancyExample "fancyStar2" 3 10 "blue"
    writeFancyExample "fancyStar3" 5 21 "yellow"
    writeFancyExample "fancyStar4" 8 30 "purple"
