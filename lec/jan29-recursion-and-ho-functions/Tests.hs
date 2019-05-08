module Tests where

import TableTagsSol

row1 = Element Tr
        [ Element Td
            [Raw "hi" ],
          Element Td
            [Raw "bye" ]]

row2 = Element Tr
        [ Element Td
            [Raw "open" ],
          Element Td
            [Raw "shut" ]]

-- tableOk should accept this one
testTable :: TableHtml
testTable =
  Element Table
    [ row1 ,
      row2]

-- the Td has a Table inside it, which is actually allowed.  So this should pass tableOk
okTable :: TableHtml
okTable = Element Table [ Element Tr [ Element Td [ Element Table []]]]
     
-- we will disallow tables with rows of different lengths (though browsers are ok with this).  So tableOk should return False for this one
badTable :: TableHtml
badTable = Element Table [ Element Tr [ Element Td [ Raw "hi" ]],
                           Element Tr [ Element Td [ Raw "there"] , Element Td [ Raw "fun"]]]

anotherBad :: TableHtml
anotherBad = Element Td [ Raw "missing table tag at start" ]
