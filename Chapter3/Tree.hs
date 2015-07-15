data Tree a = Node a (Tree a) (Tree a)
            | Empty
                deriving (Show)

data Mtree a = MNode a (Maybe (Mtree a)) (Maybe (Mtree a))
                deriving (Show)

simpleTree = Node "parent"  (Node "left child" Empty Empty) 
                            (Node "right child" Empty Empty)

mSimpleTree = MNode "parent" (Just (MNode "left child" Nothing Nothing))
                             (Just (MNode "right child" Nothing Nothing))

