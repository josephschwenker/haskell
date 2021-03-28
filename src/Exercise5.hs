module Exercise5 where

{-------------------------------------------------------------------------------

Part (a): Multisets

The AVL tree operations, below, can be viewed as providing the interface of a
*set* of elements:

 - member x s returns True if x is an element of s, False otherwise.

 - insert x s adds element x to set s

 - delete x s removes element x from set s

For the first part of this exercise, adapt the type of trees and the operations
to provide the interface of a *multiset* of elements---that is to say, a set in
which individual elements can occur multiple times.  Your new interface should
behave as follows:

 - member x s returns the number of occurrences of x in s (may be 0)

 - insert x s adds another occurrence of x to s

 - delete x s removes an occurrence of x from s

-------------------------------------------------------------------------------}
data Tree a = Leaf | Branch Int a (Tree a) (Tree a)
  deriving Show

member :: Ord a => a -> Tree a -> Bool
member x Leaf  = False
member x (Branch _ y l r)
    | x < y  = member x l
    | x == y = True
    | x > y  = member x r

member' :: Ord a => a -> Tree a -> Int
member' x Leaf  = 0
member' x (Branch _ y l r)
    | x < y  = 0 + member' x l
    | x == y = 1 + member' x l + member' x r
    | x > y  = 0 + member' x r   

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = (Branch 1 x Leaf Leaf)
insert x (Branch j y l r)
    | x < y    = balance y (insert x l) r
    | x == y   = Branch j x l r
    | x > y    = balance y l (insert x r)

insert' :: Ord a => a -> Tree a -> Tree a
insert' x Leaf = (Branch 1 x Leaf Leaf)
insert' x (Branch j y l r)
    | x < y    = balance y (insert' x l) r
    {-- Copy the line above --}
    | x == y   = balance y (insert' x l) r
    | x > y    = balance y l (insert' x r)

height :: Tree a -> Int
height Leaf             = 0
height (Branch h _ _ _) = h

balance :: Ord a => a -> Tree a -> Tree a -> Tree a
balance x l r
    | abs (hl - hr) < 2 = Branch (max hl hr + 1) x l r
    | hl > hr           = rotateR x l r
    | otherwise         = rotateL x l r
    where hl = height l
          hr = height r

rotateL :: Ord a => a -> Tree a -> Tree a -> Tree a
rotateL x l (Branch rh y rl rr)
    | rlh < rrh = Branch rh y (Branch rrh x l rl) rr
    | otherwise = Branch rh z (Branch rlh x l rll) (Branch rlh y rlr rr)
    where rlh                = height rl
          rrh                = height rr
          Branch _ z rll rlr = rl

rotateR :: Ord a => a -> Tree a -> Tree a -> Tree a
rotateR x (Branch lh y ll lr) r
    | lrh < llh = Branch lh y ll (Branch (lrh + 1) x lr r)
    | otherwise = Branch lh z (Branch llh y ll lrl) (Branch lrh x lrr r)
    where llh                = height ll
          lrh                = height lr
          Branch _ z lrl lrr = lr

deleteMin :: Ord a => Tree a -> (a, Tree a)
deleteMin Leaf = error "deleteMin: empty tree"
deleteMin (Branch h x Leaf r) = (x, r)
deleteMin (Branch h x l r) = (y, balance x l' r)
    where (y, l') = deleteMin l

combine :: Ord a => Tree a -> Tree a -> Tree a
combine l Leaf = l
combine l r = balance z l r'
    where (z, r') = deleteMin r

delete :: Ord a => a -> Tree a -> Tree a
delete _ Leaf = Leaf
delete x (Branch _ y l r)
    | x < y = balance y (delete x l) r
    | x == y = combine l r
    | otherwise = balance y l (delete x r)

delete' :: Ord a => a -> Tree a -> Tree a
delete' _ Leaf = Leaf
delete' x (Branch _ y l r)
    | x < y = balance y (delete' x l) r
    | x == y = combine l r
    | otherwise = balance y l (delete' x r)
{-------------------------------------------------------------------------------

Part (b): Flattening

Define the function flattenThen, such that flattenThen t xs produces an in-order
traversal of t, followed by xs.  Your function should be linear in the number of
(instances of) elements in t.

-------------------------------------------------------------------------------}

flattenThen :: Tree a -> [a] -> [a]
flattenThen = undefined

flatten :: Tree a -> [a]
flatten = flip flattenThen []

-- >>> flatten $ foldr insert Leaf "qwerkjasdfjewrtiuasdlkfjcxviowe"
-- "aacddeeeffiijjjkkloqrrsstuvwwwx"

{-------------------------------------------------------------------------------

Part (c): Unflattening

Define the function unflatten, such that if xs is in sorted order (but may
contain duplicates), unflatten xs produces a valid AVL tree corresponding to xs.
Your function should be linear in the length of xs.

-------------------------------------------------------------------------------}

unflatten :: Ord a => [a] -> Tree a
unflatten xs = undefined

-- >>> let t = unflatten "aacddeeeffiijjjkkloqrrsstuvwwwx"
-- >>> member 'j' t
-- 3
-- >>> member 'y' t
-- 0