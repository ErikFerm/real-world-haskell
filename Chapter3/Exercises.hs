-- Exercise 1 & 2
listLength :: Num b => [a] -> b
listLength []       = 0
listLength (x:xs)   = 1 + listLength xs

-- Exercise 3
-- first create a sum function. sum already exists so we name it sum'
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs 

-- Naive implementation (Needs to traverse the list 2 times:
-- 1 for sum' and 1 for listLength, can only handle fractals)
listMean :: Fractional a => [a] -> a
listMean [] = 0
listMean a  = (sum' a) / ((listLength a))

-- Better implementation only traverses the list once as well as
-- handle list of any real number (instead of only Fractionals)
betterListMean :: (Real a, Fractional b) => [a] -> b
betterListMean x = (realToFrac (fst (sumAndLength x))) / (snd (sumAndLength x)) 
    where sumAndLength []       = (0,0) 
          sumAndLength (x:xs)   = (x + fst y, 1.0 + snd y)
            where y = sumAndLength xs

-- Exercise 4 & 5
-- Create reverse function
r :: [a] -> [a]
r (x:xs) = r xs ++ [x]
r [] = []

-- Create plindrome function
pal :: [a] -> [a]
pal l = l ++ r l

-- Create check if a list is a palindrome
isPal :: (Eq a) => [a] -> Bool
isPal l = l == (r l)

-- Exercise 6
-- Solved by a quicksort implementation using list comprehension
sortLoL :: [[a]] -> [[a]]
sortLoL [] = []
sortLoL (x:xs) = shorter ++ [x] ++ longer
    where shorter = sortLoL [a | a <- xs, length a <= length x]
          longer  = sortLoL [a | a <- xs, length a > length x]

-- Exercise 7
intersperse' :: a -> [[a]] -> [a]
intersperse' s (x:xs)   = x ++ y 
    where y = if null xs
              then []
              else s:intersperse' s xs

-- Excersise 8
data Mtree a = MNode a (Maybe (Mtree a)) (Maybe (Mtree a))
                deriving (Show)
mSimpleTree = Just (MNode "parent" (Just (MNode "left child" (Just (MNode "left childs left child" Nothing Nothing)) Nothing))
                             (Just (MNode "right child" Nothing Nothing)))

treeHeigth :: Maybe (Mtree t) -> Int
treeHeigth t = 
    case t of
        Nothing                     -> 0
        Just (MNode _ left right)   -> 1 + max (treeHeigth left) (treeHeigth right)

-- Exercise 9 & 10 & 11
data Direction = DLeft | DRight | DStraight
    deriving (Show,Eq)

data Point2D = Point2D {
                x :: Double,
                y :: Double
                } deriving (Show, Eq)

type Vector = Point2D

createVector:: Point2D -> Point2D -> Vector
createVector p1 p2 = Point2D ((x p2) - (x p1)) ((y p2)-(y p1))

normalizeVector :: Vector -> Vector
normalizeVector (Point2D x y)  = Point2D (x/n) (y/n)
  where n = (sqrt ((x)^2 + (y)^2))

dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 = (x v1) * (x v2) 
                 + (y v1) * (y v2)

calculateTurn :: [Point2D] -> [Direction]
calculateTurn (p1:p2:p3:ps)
  | acos (dotProduct v1 vC) >  acos (dotProduct v2 vC) = DRight:calculateTurn (p2:p3:ps)
  | acos (dotProduct v1 vC) <  acos (dotProduct v2 vC) = DLeft:calculateTurn (p2:p3:ps)
  | acos (dotProduct v1 vC) == acos (dotProduct v2 vC) = DStraight:calculateTurn (p2:p3:ps)                                        
    where v1 = normalizeVector (createVector p1 p2)
          v2 = normalizeVector (createVector p2 p3)
          vC = Point2D 1 0
calculateTurn _ = []

ccw :: [Point2D] -> Double
ccw [p1,p2,p3] = (x p2 - x p1)*(y p3 - y p1) - (y p2 - y p1)*(x p3 - x p1)

create2dPoints :: [Double] -> [Double] -> [Point2D]
create2dPoints (x:xs) (y:ys)  = (Point2D x y):create2dPoints xs ys
create2dPoints _ _            = [] 

printP2DasListOfTuples (p:ps) = (x p , y p):printP2DasListOfTuples ps
printP2DasListOfTuples _      = []

listOfThreePoints = [Point2D 0 0, Point2D 1 0, Point2D 2 (-1)]
l = create2dPoints [-3,-5,0,4,5,0,-3,2,-5,5] [-5,-1,1,-3,4,-5,1,-5,-2,0]


-- Exercise 12
findLowestPoint :: [Point2D] -> Point2D
findLowestPoint (p1:p2:ps)
  | (y p1) <  (y p2) = findLowestPoint (p1:ps) 
  | (y p1) >  (y p2) = findLowestPoint (p2:ps)
  | (y p1) == (y p2) = 
    if (x p1) < (x p2)
    then findLowestPoint (p1:ps)
    else findLowestPoint (p2:ps)
findLowestPoint [p] = p 

sortPoints :: [Point2D] -> Point2D -> [Point2D]
sortPoints (p:ps) ip = smaller ++ [p] ++ larger
  where 
    smaller = sortPoints [x | x <- ps, test x <= test p] ip
    larger  = sortPoints [x | x <- ps, test x > test p] ip
    test a
      | a == ip   = -0.1
      | otherwise = acos $ dotProduct (xAxis) (normalizeVector $ createVector ip a)
    xAxis = Point2D 1 0
sortPoints [] ip = []

grahamScan :: [Point2D] -> [Point2D]
grahamScan l = calculate $ sortPoints l $ findLowestPoint l
  where 
    calculate (p1:p2:p3:ps)
      | ccw [p1,p2,p3] > 0 = p1:calculate (p2:p3:ps)
      | otherwise          = calculate (p1:p3:ps)
    calculate [p1,p2]
      | ccw [p1,p2,head l] > 0 = [p1,p2]
      | otherwise              = []
    calculate _ = []


