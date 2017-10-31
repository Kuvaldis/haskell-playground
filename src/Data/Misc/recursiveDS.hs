-- :-: would be a function, name of which consists only of special characters. This fact makes :-: infix
-- infixr makes it right associative (like in the case of plain lists '1 : []')
-- number 5 means how tightly the operator binds, e.g. for * it's 7, for + it's 6, so in expression '1 + 2 * 3' even though both operators are left associative, * will be resolved first
-- without this number + would be resolved first, then *, which is not correct
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty      .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

-- binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a leftSubtree rightSubtree)
    | x == a = Node x leftSubtree rightSubtree
    | x <  a = Node a (treeInsert x leftSubtree) rightSubtree
    | x >  a = Node a leftSubtree (treeInsert x rightSubtree)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x <  a = treeElem x left
    | x >  a = treeElem x right

treeFromList :: (Ord a) => [a] -> Tree a
-- takes treeInsert, EmptyTree as starting value and folds from right to left in the list nums
treeFromList nums = foldr treeInsert EmptyTree nums
