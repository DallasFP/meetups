module Main where

import           Data.Tree
import           Data.Tree.Pretty

poly :: Tree String
poly =
  Node "P"
  [ Node "O"
    [ Node "L"
      [ Node "N" []
      , Node "T" []
      ]
    , Node "Y"
      [ Node "S" []
      , Node "A" []
      ]
    ]
  , Node "L"
    [ Node "W"
      [ Node "C" []
      , Node "H" []
      ]
    , Node "A"
      [ Node "I" []
      , Node "P" []
      ]
    ]
  ]

-- do it with pattern matching

changePatMatch :: Tree String -> Tree String
changePatMatch (Node p [o, Node l [Node _ ch, a]]) = Node p [o, Node l [Node "XXX" ch, a]]
changePatMatch a = a

-- it's not pretty or fun
-- hard to re-use
-- fixed definition of change
-- we'd like something more flexible
-- what about a list of instructions on how to traverse the tree?

--------------------------------------------------------------------------------

data Direction = L | R
  deriving Show

changeDirections :: [Direction] -> Tree String -> Tree String

changeDirections (L:ds) (Node v (l:r)) = Node v (changeDirections ds l : r)
changeDirections (L:_ ) (Node v []   ) = Node v []

changeDirections (R:ds) (Node v (l:r:_)) = Node v (l:[changeDirections ds r])
changeDirections (R:_ ) (Node v l      ) = Node v l

changeDirections [] (Node _ ns) = Node "XXX" ns

-- cool
-- inefficient for repeated updates
   -- always have to start at the root
-- still neat :)

--------------------------------------------------------------------------------

-- would be nice to be able to focus on a sub tree & leave a trail (e.g. how we got there)

goLeft :: (Tree String, [Direction]) -> (Tree String, [Direction])
goLeft (Node _ (l:_), bs) = (l, L:bs)
goLeft (n           , bs) = (n, bs)

goRight :: (Tree String, [Direction]) -> (Tree String, [Direction])
goRight (Node _ (_:r:_), bs) = (r, R:bs)
goRight (n             , bs) = (n, bs)

-- ok so we have a trail
-- nice to be able to navigate
-- can't go up, can only go down =\
   -- let's remember up!

--------------------------------------------------------------------------------

-- remember Forest is [Tree]

data Crumb
  = LeftCrumb String (Forest String)
  | RightCrumb String (Forest String)
  deriving Show

left :: (Tree String, [Crumb]) -> (Tree String, [Crumb])
left (Node v (l:r), bs) = (l, LeftCrumb v r : bs)
left (n, bs) = (n, bs)

right :: (Tree String, [Crumb]) -> (Tree String, [Crumb])
right (Node v (l:r:rs), bs) = (r, RightCrumb v (l:rs) : bs)
right (n, bs) = (n, bs)

up :: (Tree String, [Crumb]) -> (Tree String, [Crumb])
up (t, LeftCrumb  v r : bs) = (Node v (t:r), bs)
up (t, RightCrumb v l : bs) = (Node v (l++[t]), bs)
up (t, []) = (t, [])

--------------------------

type Zipper = (Tree String, [Crumb])

modify :: (String -> String) -> Zipper -> Zipper
modify f (Node v ns, bs) = (Node (f v) ns, bs)

--------------------------------------------------------------------------------

attach :: Tree String -> Zipper -> Zipper
attach t' (Node v ts, bs) = (Node v (t':ts), bs)

--------------------------------------------------------------------------------

draw :: Tree String -> IO ()
draw = putStrLn.drawVerticalTree

main :: IO ()
main = draw poly
