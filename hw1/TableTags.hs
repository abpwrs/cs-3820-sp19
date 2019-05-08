module TableTags where

data TableTag
  = Table
  | Tr
  | Td
  | Th

showTableTag :: TableTag -> String
showTableTag x =
  case x of
    Table -> "table"
    Tr -> "tr"
    Td -> "td"
    Th -> "th"

equalTableTags :: TableTag -> TableTag -> Bool
equalTableTags elem_one elem_two =
  showTableTag elem_one == showTableTag elem_two

instance Show TableTag where
  show = showTableTag

{- allows syntactic equals comparison -}
instance Eq TableTag where
  (==) = equalTableTags

directElt :: TableTag -> TableTag -> Bool
directElt Tr Table = True
directElt Td Tr = True
directElt Th Tr = True
directElt Table Td = True
directElt Table Th = True
directElt _ _ = False

{- TableHtml

   The object

     Element tag [ subelt_1 , ... , subelt_n ]

   represents an HTML element with the given tag and the given sub elements.  n is allowed to be 0, which
   is one base case for TableHtml.

   The other base case for TableHtml is

     Raw s

   which just represents unstructured textual data s as part of TableHtml. -}
data TableHtml
  = Element TableTag
            [TableHtml]
  | Raw String

showTable :: TableHtml -> String
showTable (Raw s) = s
showTable (Element tag list) =
  "<" ++
  showTableTag tag ++
  ">" ++ concatMap showTable list ++ "<" ++ showTableTag tag ++ "/>"

instance Show TableHtml where
  show = showTable

{- return a list of the sub elements (like subelt_1 , ... , subelt_n above).
   a Raw object should have the empty list of sub elements. -}
getSubelts :: TableHtml -> [TableHtml]
getSubelts (Element _ val) = val
getSubelts (Raw _) = []

{- return True iff the given TableHtml is an Element with the given TableTag.  You can use == to compare TableTags,
   because the code above makes TableTag an instance of the Eq type class. -}
hasTag :: TableHtml -> TableTag -> Bool
hasTag (Raw _) _ = False
hasTag (Element tag list) search_tag =
  search_tag == tag || foldr ((||) . (`hasTag` search_tag)) False list

{- return the number of subelts of the given TableHtml.  The intention is that this function will just be called
   with TableHtml whose tag is Tr, but correct operation of the function does not require this. -}
rowLength :: TableHtml -> Int
rowLength (Raw _) = 0
rowLength (Element _ list) = length list + sum (map rowLength list)

tableTagCheck :: TableHtml -> Bool
tableTagCheck (Element tag list) = tag == Table

tableRelationsCheck :: TableHtml -> Bool
tableRelationsCheck (Raw _) = True
tableRelationsCheck (Element tag list) =
  all (auxFunc tag) list && all tableRelationsCheck list

auxFunc :: TableTag -> TableHtml -> Bool
auxFunc tag (Raw _) = True
auxFunc tag (Element x _) = directElt x tag

tableRowLengthCheck :: TableHtml -> Bool
tableRowLengthCheck (Raw _) = True
tableRowLengthCheck (Element _ list) =
  all (== (magicFunc (head list))) (map magicFunc list) &&
  all tableRowLengthCheck list

magicFunc :: TableHtml -> Int
magicFunc (Raw _) = -1
magicFunc (Element elem list) =
  case elem of
    Tr -> rowLength (Element elem list)
    _ -> -1

tableOk :: TableHtml -> Bool
tableOk (Raw _) = False

tableOk table =
  tableTagCheck table && tableRelationsCheck table && tableRowLengthCheck table
