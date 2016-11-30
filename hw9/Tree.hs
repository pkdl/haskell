import Prelude hiding (lookup)

data BinaryTree k v = Leaf
            	  	| Node k v (BinaryTree k v) (BinaryTree k v)
            	  	deriving (Eq,Ord,Show,Read)

lookup :: Ord k => k -> BinaryTree k v -> Maybe v
lookup _ Leaf = Nothing
lookup x (Node key value l r)
		| x == key = Just value
		| x < key = lookup x l
		| x > key = lookup x r

insert :: Ord k => k -> v -> BinaryTree k v -> BinaryTree k v
insert k v Leaf = Node k v Leaf Leaf
insert k v (Node key value l r)
		| k == key = Node k v l r
		| k > key = Node key value l (insert k v r)  
		| k < key = Node key value (insert k v l) r

delete :: Ord k => k -> BinaryTree k v -> BinaryTree k v
delete _ Leaf = Leaf
delete k (Node key value l r)
		| k == key = resolve (Node key value l r)
		| k > key = Node key value l (delete k r)
		| k < key = Node key value (delete k l) r

resolve :: Ord k => BinaryTree k v -> BinaryTree k v
resolve (Node key value Leaf r) = r
resolve (Node key value l Leaf) = l
resolve (Node key value l r) = nodeFromTuple (findKey r) l r

nodeFromTuple :: Ord k => (k, v) -> BinaryTree k v -> BinaryTree k v -> BinaryTree k v
nodeFromTuple (k, v) l r = Node k v l (delete k r)

findKey :: Ord k => BinaryTree k v -> (k, v)
findKey (Node k v Leaf _) = (k, v)
findKey (Node k v l _) = findKey l