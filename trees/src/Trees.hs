module Trees where

data Tree a = Empty 
             | Root a [Tree a] deriving (Show, Eq)

depth :: Tree a -> Int
depth Empty = 0
depth (Root root []) = 1
depth (Root root children) = 1 + foldl1 max (map depth children)
    
buildTree :: a -> [Tree a] -> Tree a
buildTree root children = Root root children

appendChildren :: Tree a -> [Tree a] -> Tree a
appendChildren (Root root _) children = Root root children

rootOf (Root root _) = root
childrenOf (Root _ children) = children
nthChildren (Root _ children) nth = children !! nth

leaf :: a -> Tree a
leaf root = Root root []

unfoldTree :: a -> [a -> a] -> Int -> Tree a
unfoldTree root [] depth = leaf root
unfoldTree root fns 0 = leaf root
unfoldTree root fns n = Root root childrenTrees where
    childrenTrees = map (\child -> unfoldTree child fns (n-1)) children
    children = map (\fn -> fn root) fns

pascal :: Int -> Tree Integer
pascal 1 = leaf 1
pascal 2 = appendChildren (pascal 1) [leaf 1, leaf 2, leaf 1]
pascal 3 = appendChildren pascal2 (unfoldPairs pascal2 (leaf 1) (\[l,r] -> leaf ((rootOf l) + (rootOf r)))) where
    pascal2 = pascal 2
    unfoldPairs tree boundary expansionFunction = boundary : expansion ++ [boundary] where
        expansion = map expansionFunction (pair tree)

-- TODO: This needs to return a tree, with the element as the root and the pair as the children.
-- Or the index as the root and the pair as the children. Limitation: only applies to Tree Integer (which is OK as Pascal's triangle is Tree Integer)
pair :: Tree a -> [[Tree a]]
pair (Root root children) = reverse $ snd $ foldl
    (\acc ele -> (ele, [fst acc,ele]:(snd acc)))
        ((head children),[]) $ tail children
