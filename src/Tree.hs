module Tree
  where

import Queue

data Tree a = Leaf a
              | Root a [Tree a] 
  deriving (Show, Eq)

countTree::Tree a->Int
countTree (Leaf a) =1
countTree (Root a subT)=1+ sum (map countTree subT)

mapTree::(a->b)->Tree a->Tree b
mapTree f (Leaf a)=Leaf (f a)
mapTree f (Root a subT)=Root (f a) (map (mapTree f) subT)

height::Tree a->Int
height (Leaf a)=1
height (Root a subT)=1 + (maximum (map height subT))

maxTree::(Ord a)=>Tree a->a
maxTree (Leaf a)=a
maxTree (Root a subT)=maximum (a:(map maxTree subT))
--or maxTree (Root a subT)= max a $ maximum $ map maxTree subT

--Level Order Trsversal (For Tree a)

levelOrderTraversal :: Tree a -> [a]
levelOrderTraversal t = helperLOT (makeQueue [t]) []

helperLOT :: Queue (Tree a) -> [a] -> [a]
helperLOT q acc = case dequeue q of
  Left _ -> reverse acc
  Right (headTree,newQueue) -> case headTree of
    Leaf a -> helperLOT newQueue (a:acc)
    Root a xs -> helperLOT (bulkEnque newQueue xs) (a:acc)

{-roundOrderTraversal :: Tree a -> [a]
roundOrderTraversal t = hRot (makeQueue [t]) [] 0

hRot :: Queue (Tree a) -> [a] -> Int -> [a]
hRot q acc l = case dequeue q of
  Left _ -> reverse acc
  Right (headTree,nQueue) -> case headTree of
   Leaf a -> hRot nQueue (a:acc) l
   Root a xs -> hRot (bulkEnque nQueue (reverse xs)) (a:acc) 
-}

--Connected Level Order Traversal
cLot :: Tree a -> [a]
cLot tree@(Root a subT) = [a] ++ (concat $ tReverse (map getAllValues $ subClot tree) 1)

tReverse :: [[a]] ->Int -> [[a]]
tReverse [[]] _ = [[]]
tReverse [] _= []
tReverse (x:xs) a = (if odd  a then (reverse x) else x): tReverse xs (a+1)

subClot :: Tree a -> [[Tree a]]
subClot (Leaf a) = [[]]
subClot t@(Root a subT) = (getAllSubT [t] : (concat $ map subClot subT))

getValue :: Tree a -> a
getValue (Leaf a) = a
getValue (Root a subT) = a

getAllValues :: [Tree a] -> [a]
getAllValues [] = []
getAllValues trees = map getValue trees

getSubT :: Tree a -> [Tree a]
getSubT (Leaf a) = []
getSubT (Root a subT) = subT

getAllSubT :: [Tree a] -> [Tree a]
getAllSubT trees = concat $ map getSubT trees

tree = Root 2 [Leaf 3]
tree1 = Root 2 [Root 5 [Root 9 [Leaf 1]]]
tree2 = Root 1 [Root 2 [Root 3 [Leaf 6],Leaf 4],Leaf 5]
tree3 = Root 1 [Root 2 [Root 5 [Root 9 [Leaf 12]]],Root 3 [Leaf 6,Root 7 [Leaf 10,Leaf 11]],Root 4 [Leaf 8]]
tree4 = Root 1 [Root 2 [Leaf 4, Leaf 5],Root 3 [Leaf 6, Leaf 7]]
-- Functions for Binary Tree

data BinaryTree a = EmptyNode
                  | Node a (BinaryTree a) (BinaryTree a)
  deriving (Show,Eq)

et = EmptyNode

btCount::BinaryTree a -> Int
btCount (EmptyNode) = 0
btCount (Node a lSubT rSubT) = 1 + btCount lSubT + btCount rSubT

btMap::(a->b)-> BinaryTree a -> BinaryTree b
btMap f (EmptyNode) = EmptyNode
btMap f (Node a lSubT rSubT)= Node (f a) (btMap f lSubT) (btMap f rSubT)


-- For finding maximum value from BinaryTree
btMax :: (Ord a) => BinaryTree a -> a
btMax (Node a EmptyNode EmptyNode) = a
btMax (Node a EmptyNode b) = max a $ btMax b
btMax (Node a b EmptyNode ) = max a $ btMax b
btMax (Node a lSubT rSubT) = maximum ([a]++[btMax lSubT]++[btMax rSubT])

btHeight :: BinaryTree a -> Int
btHeight (EmptyNode) = 0
btHeight (Node a lSubT rSubT) = 1 + maximum ((btHeight lSubT):[btHeight rSubT])

--In Order Treversal
ioT :: BinaryTree a -> [a]
ioT (EmptyNode) = []
ioT (Node a lSubT rSubT) = ioT lSubT ++ [a] ++ ioT rSubT

--Pre Order Traversal
poT :: BinaryTree a -> [a]
poT (EmptyNode) = []
poT (Node a lSubT rSubT) = [a] ++ poT lSubT ++ poT rSubT

--Post Order Traversal
posT :: BinaryTree a -> [a]
posT (EmptyNode) = []
posT (Node a lSubT rSubT) = posT lSubT ++ posT rSubT ++ [a]

bt1=Node 2 (Node 8 et et) et
bt2=Node 1 (Node 2 (Node 3 et et) et) (Node 4 et (Node 5 et et))
bt3=(Node 2 (Node 4 et (Node 6 et et)) (Node 8 (Node 10 et et) (Node 0 et et)))

--chk = loT tree2 == ans2
--r = loT tree2 

listMax ::(Ord a) => [a] -> a
listMax [a] = a
listMax (x:xs) = max x $ listMax xs
