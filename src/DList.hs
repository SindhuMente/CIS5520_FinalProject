module DList where

type DList a = [a] -> [a]

list :: [Int]
list = [1, 2, 3] -- end is nil

dlist :: DList Int
dlist = \x -> 1 : 2 : 3 : x -- end is "x"

toList :: DList a -> [a]
toList x = x []

-- \| Create an empty DList
-- >>>toList empty
-- []
empty :: DList a
empty = id -- or \x -  x

-- \| Create a DList containing a single element
-- >>> toList (singleton "a")
-- ["a"]
singleton :: a -> DList a
singleton = (:)

-- \| Append two DLists together
-- >>> toList ((singleton "a") `append` (singleton "b"))
-- ["a","b"]
append :: DList a -> DList a -> DList a
append = (.)

-- \| Construct a DList from a head element and tail
-- >>> toList (cons "a" (singleton "b"))
-- ["a","b"]
cons :: a -> DList a -> DList a
cons x y = singleton x `append` y -- use definitions above
-- or cons = (.) . (:)             -- for maximum obscurity
-- Reminder: (.) f g = \x -  f (g x)   -- definition of (.)
--
-- cons = (.) . (:)
--      = \x -  (.) ((:) x)
--      = \x -  (.) (\l' -  x : l')
--      = \x -  \f -  \l -  (\l' -  x : l') (f l)
--      = \x -  \f -  \l -  x : f l
-- i.e.
-- cons x f = \l -  x : f l

fromList :: [a] -> DList a
fromList = foldr cons empty
