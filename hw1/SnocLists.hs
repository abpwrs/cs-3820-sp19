module SnocLists where

data SList a
  = Snil
  | Scons (SList a) a

showSList :: (Show a) => SList a -> String
showSList Snil = ""
showSList (Scons list elem) = show elem ++ showSList list

instance (Show a) => Show (SList a) where
  show = showSList

equalSLists :: (Eq a) => SList a -> SList a -> Bool
equalSLists Snil Snil = True
equalSLists _ Snil = False
equalSLists Snil _ = False
equalSLists (Scons list_one elem_one) (Scons list_two elem_two) =
  elem_one == elem_two && equalSLists list_one list_two

instance (Eq a) => Eq (SList a) where
  (==) = equalSLists

sappend :: SList a -> SList a -> SList a
-- Base Cases
sappend (Scons a b) Snil = Scons a b
sappend Snil (Scons a b) = Scons a b
sappend Snil Snil = Snil
-- magic recursive line here
sappend (Scons list elem) slist =
  case list of
    Snil -> Scons slist elem
    (Scons a b) -> Scons (Scons (sappend a slist) b) elem

slength :: SList a -> Int
-- Base Case
slength Snil = 0
-- magic recursive line here
slength (Scons list a) = 1 + slength list

smap :: (a -> b) -> SList a -> SList b
smap f Snil = Snil
smap f (Scons a b) = Scons (smap f a) (f b)

sfilter :: (a -> Bool) -> SList a -> SList a
sfilter predicate Snil = Snil
sfilter predicate (Scons list elem) =
  if predicate elem
    then Scons (sfilter predicate list) elem
    else sfilter predicate list

sintersperse :: (Eq a) => a -> SList a -> SList a
sintersperse _ Snil = Snil
sintersperse add_elem (Scons list elem)
  | list == Snil = Scons Snil elem
  | otherwise = Scons (Scons (sintersperse add_elem list) add_elem) elem

-- given a list of lists l1 ... lk , return the concatenation of l1 through lk.
sconcat :: SList (SList a) -> SList a
sconcat Snil = Snil
sconcat (Scons list_of_lists one_list) = sappend one_list (sconcat list_of_lists)

