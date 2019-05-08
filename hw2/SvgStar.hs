module SvgStar where

import Data.List

type Point = (Int,Int)
type Edge = (Point, Point)

htmlStart :: String
htmlStart = "<!DOCTYPE html>\n"++
           "<html>\n"++
           "<body>\n\n"++
           "<svg width=\"1000\" height=\"1000\">\n\n"

htmlEnd :: String
htmlEnd = "</svg>\n</body>\n</html>\n"

drawLine :: Int -> Int -> Int -> Int -> String
drawLine x1 y1 x2 y2 = "<line x1 =\"" ++ show x1 ++ "\" y1 = \"" ++ show y1 ++ "\" x2 = \"" ++ show x2 ++ "\" y2 = \"" ++ show y2 ++ "\" style=\"fill:none;stroke:black\"/>"

drawList :: [Edge] -> String
drawList [] = ""
drawList (((x1, y1),(x2, y2)):xs) = (drawLine x1 y1 x2 y2) ++ "\n" ++ drawList xs

translateAll :: Int -> Int -> [Point] -> [Point]
translateAll tx ty l = map (translate tx ty) l

translate :: Int -> Int -> Point -> Point
translate tx ty (x, y) = (x + tx, y + ty) 

evenlySpacedPoints :: Int -> Int -> Int -> [Point]
evenlySpacedPoints n k r 
  | k == 0 = []
  | otherwise = (round ((fromIntegral r) * cos(((fromIntegral k) * 2 * pi) / fromIntegral(n))), round ((fromIntegral r) * sin(((fromIntegral k) * 2 * pi / fromIntegral(n))))) : (evenlySpacedPoints n (k-1) r)

edgesFromPoints :: Int -> Int -> [Point] -> [Edge]
edgesFromPoints sep i l
  | i == length l = []
  | otherwise = (l!!i, l!!((i + sep) `mod` (length l))) : edgesFromPoints sep (i + 1) l

showStar :: Int -> Int -> Int -> Int -> Int -> String
showStar tx ty r sep n = drawList (edgesFromPoints sep 0 (translateAll tx ty (evenlySpacedPoints n n r)))

writeStar :: String -> Int -> Int -> Int -> Int -> Int -> IO ()
writeStar filename tx ty r sep n =
  writeFile filename (htmlStart ++ showStar tx ty r sep n ++ htmlEnd)
