module Tests where

import SvgStar

writeExample :: String -> Int -> Int -> IO ()
writeExample file = writeStar (file ++ ".html") 300 300 200 

main :: IO ()
main =
  do
    writeExample "star1" 2 6
    writeExample "star2" 3 10
    writeExample "star3" 5 21
    writeExample "star4" 8 30        
