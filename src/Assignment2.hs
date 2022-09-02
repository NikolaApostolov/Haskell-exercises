import Test.QuickCheck
import Data.Bifunctor
instance Show (a -> b) where
         show a= "function"
         
{-Assignment 2 is due 25/03/22 at 17:00. You should submit your solutions in the format <your name>.hs using the submission link in the week 10 materials. Your submission must be your own work. Partial solutions may still attract partial marks. You may not change the types of any of the functions given, altering them will result in lost marks. You may use any function from the Haskell prelude however you may not import other libraries unless told otherwise.-}

data Move = M1 | M2
    deriving(Eq, Show)


{-1) A move is said to be optimal if the player can never improve their payoff by changing their move regardless of their opponents move. 
    Define isOptimal which checks that a move is optimal, that the value returned is higher than the alternative move for all of the opponents moves.(2 marks)-}

isOptimal::Move -> ((Move,Move) -> Int) -> Bool
isOptimal playerMove f = f (playerMove, playerMove) > f (altMove playerMove, playerMove) && f (playerMove, altMove playerMove) > f (altMove playerMove, altMove playerMove)

difMoves:: (Move,Move) -> Int
difMoves (M1,M1) = 2
difMoves (M2,M1) = 4
difMoves (M1,M2) = 1
difMoves (M2,M2) = 3

altMove:: Move -> Move
altMove M1 = M2
altMove M2 = M1
     
{-2) A truth table for a 2 bit logic gate evaluates the gate on all possible inputs. Define truthTable which takes a two bit logic gate and prints its truthTable. Your function should work for any 2 bit logic gate. (2 marks)
Your ttable should appear as  X Y | fun x y
                              O O | O
                              O I | O
                              I O | O
                              I I | I       -}



data Bit = I | O
    deriving(Show, Eq)


truthTable::(Bit -> Bit -> Bit) -> IO()
truthTable f = do putStr "X Y | fun x y \n"
                  putStr "O O | " 
                  putStr (bitToStr (f O O))
                  putStr "\n"
                  putStr "O I | " 
                  putStr (bitToStr (f O I))
                  putStr "\n"
                  putStr "I O | " 
                  putStr (bitToStr (f I O))
                  putStr "\n"
                  putStr "I I | " 
                  putStr (bitToStr (f I I))
                  putStr "\n"

bitToStr::Bit -> String
bitToStr O = "O"
bitToStr I = "I"

myAnd::Bit -> Bit -> Bit
myAnd O _ = O
myAnd _ O = O
myAnd _ _ = I

myOr::Bit -> Bit -> Bit
myOr I _ = I
myOr _ I = I
myOr _ _ = O

{-3a) Consider the following functions. By testing them and deciphering their purpose define quickCheck properties using prelude functions to test their behaviour appropriately. Your tests should pass if you have appropriately defined test cases. Note that testing trivial conditions does not count as an appropriate test case. (1 mark per test case)-}

myFun1::Int -> Char -> String
myFun1 0 a = [] 
myFun1 n a | n < 0 = [] 
           | otherwise = a:(myFun1 (n-1) a)

prop_myFun1 n a | n > 0 = length (myFun1 n a) == n
                | otherwise = length (myFun1 n a) == 0


myFun2::[a] -> [b] -> (a -> c) -> (b -> c) -> [(c,c)]
myFun2 [] _ f g = []
myFun2 _ [] f g = []
myFun2 (x:xs) (y:ys) f g = (f x, g y):myFun2 xs ys f g

prop_myFun2 as bs f g = zip (map f as) (map g bs) == myFun2 as bs f g

myFun3::[a] -> (b -> a -> b) -> b -> [b]
myFun3 [] f b = [b]
myFun3 (x:xs) f b = (b):myFun3 xs f (f b x)

prop_myFun3 a f b = length a + 1 == length (myFun3 a f b)

--3b) Write a short IO function, runTest, which runs your tests using quickCheck. (2 marks)
test1 = quickCheck prop_myFun1
test2 = quickCheck (prop_myFun2::[Int] -> [Int] -> (Int -> Int) -> (Int -> Int) -> Bool)
test3 = quickCheck (prop_myFun3::[Int] -> (Int -> Int -> Int) -> Int -> Bool)
runTest = do sequence [test1, test2, test3]

--Consider the following type which allows non-homogenous lists to be built.

data MkType a b = ElemA a (MkType a b) | ElemB b (MkType a b) | Empty
 deriving Show

--4a) Define collapse which reduces a list of MkType a b to two lists of type a and b respectively. You should respect the orignal ordering of the list (2 marks).

collapse::MkType a b -> ([a],[b])
collapse Empty = ([],[])
collapse abs = (flatA abs, flatB abs)

flatA:: MkType a b -> [a]
flatA Empty = []
flatA (ElemB b abs) = flatA abs
flatA (ElemA a abs) = a:(flatA abs)

flatB:: MkType a b -> [b]
flatB Empty = []
flatB (ElemA a abs) = flatB abs
flatB (ElemB b abs) = b:(flatB abs)

--4b) Define sortAB which sorts a list of type MkType a b. Your function should not change the ordering of ElemA ElemB constructors but it should sort the list so that elements of a are in ascending order as are elements of type b. e.g 
    --sortAB (ElemA 5 (ElemB 'z' (ElemB 'c' (ElemA 4 (ElemB 'a' Empty))))) = (ElemA 4 (ElemB 'a' (ElemB 'c' (ElemA 5 (ElemB 'z' Empty))))) (3 marks).

sortAB::(Ord a,Ord b) => MkType a b -> MkType a b
sortAB Empty = Empty
sortAB abs = constrAs (quickSort (flatA abs)) (constrBs (quickSort (flatB abs)) abs)

quickSort::Ord a => [a] -> [a]
quickSort [] = []
quickSort (a:as) = quickSort (filter (<a) as) ++ [a] ++ quickSort (filter (>=a) as)

constrAs::Ord a => [a] -> MkType a b -> MkType a b
constrAs [] abs = abs
constrAs _ Empty = Empty
constrAs (p:ps) (ElemA a abs) = ElemA p (constrAs ps abs)
constrAs ps (ElemB b abs) = ElemB b (constrAs ps abs)

constrBs::Ord a => [b] -> MkType a b -> MkType a b
constrBs [] abs = abs
constrBs _ Empty = Empty
constrBs (p:ps) (ElemB b abs) = ElemB p (constrBs ps abs)
constrBs ps (ElemA a abs) = ElemA a (constrBs ps abs)

--4c) A bifunctor is similar to a functor except that it maps two possible types. Define a Bifunctor instance for MkType by defining bimap:: Bifunctor p => (a -> b) -> (c -> d) -> p a c -> p b d (3 marks).

instance Bifunctor MkType where
    bimap f g Empty = Empty
    bimap f g (ElemA a abs) = ElemA (f a) (bimap f g abs)
    bimap f g (ElemB b abs) = ElemB (g b) (bimap f g abs)

--4d) Define multiFold which folds an element of MkType returning a tuple of type (a,b). You should define this as a right fold (3 marks). 

multiFold:: MkType a b -> (c,d) -> (a -> c -> c) -> (b -> d -> d) -> (c,d)
multiFold Empty (c,d) f g = (c,d)
multiFold (ElemA a abs) (c,d) f g = multiFold abs (foldr f c [a], d) f g
multiFold (ElemB b abs) (c,d) f g = multiFold abs (c, foldr g d [b]) f g