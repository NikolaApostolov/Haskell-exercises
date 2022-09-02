{-Attempt all questions. Make sure that your code compiles and check regularly by loading it into GHCI. You should test your code regularly and make sure that your solutions satisfy the test cases given, note that the test cases don't cover every scenario but they will give you a good idea of how you are doing. You may define as many helper functions as you like, you may also use any function from the Haskell prelude unless otherwise specified. You may not import any libraries -}

----------------------------------------------
--SUBMIT YOUR ATTEMPT BEFORE 8:00PM
----------------------------------------------

{-1a) Define eqInt which rerturns True when two supplied integers are equal and False otherwise. (1 mark)

eq 5 5 == True 
eq 4 5 == False
-}

eqInt::Int -> Int -> Bool
eqInt x y = x == y

{-1b) Define evList which, using your answer to question 1, returns True when the length of a given list is even. (2 marks)

evList "Clemens" == False
evList "lemens" == True
-}

evList::[a] -> Bool
evList a = eqInt (length a) ((length a `div` 2) * 2)

{-1c) Using evList define ifList which, if the length of a list is even, returns the first element of the list and the rest of the list otherwise. You should assume that the list is not empty. (3 marks) 

ifList [1,2,3,4,5] == Right [2,3,4,5]
ifList [1,2,3,4] == Left 1
 -}

ifList::[a] -> Either a [a]
ifList (a:as) | evList (a:as) = Left a
              | otherwise = Right as

{-2a) Using list comprehension build a list of tuples consisting of a number and it's square. Your function should go up to the value supplied as input. (2 marks)

sqList 2 == [(1,1),(2,4)] 
sqList 4 == [(1,1),(2,4),(3,9),(4,16)]
-}

sqList::Int -> [(Int,Int)]
sqList 0 = []
sqList n = sqList (n - 1) ++ [(n, n*n)]

{-2b) Define shortenList which takes a list and an element x of the same type. It then counts the number of x's in the list and removes that number of elements from the end of the list. (3 marks)

shortenList 'a' "asdaaldi" == "asdaa"
shortenList 1 [1,2,3,4,5] == [1,2,3,4]
 -}

shortenList::Eq a => a -> [a] -> [a]
shortenList a [] = []
shortenList a as = shorten (length as - count 0 a as) as

count::Eq a => Int -> a -> [a] -> Int
count n x [] = n
count n x (a:as) | x == a = count (n+1) x as
                 | otherwise = count n x as

shorten:: Int -> [a] -> [a]
shorten 0 as = []
shorten n [] = []
shorten n (a:as) = a : shorten (n-1) as

{-2c) Define reorderList which moves all occurences of a given value in a list to the end of the list. (3 marks)

reorderList 'a' "asasasd" == "sssdaaa"
reorderList 1 [1,2,1,3,4,1] == [2,3,4,1,1,1]
-}

reorderList::Eq a => a -> [a] -> [a] 
reorderList _ [] = []
reorderList a (b:bs) | a == b = reorderList a bs ++ [b]
                     | otherwise = b : reorderList a bs

{-2d) By using your solutions to 2b and 2c define remove which removes an element from a list of values. (2 marks) 

remove 'a' "asdaaldi" == "sdldi" 
remove 1 [1,2,3,5,1] == [2,3,5]
-}

remove::Eq a => a -> [a] -> [a]
remove a as = shortenList a (reorderList a as)

{-3 Consider the following type -}

data Val = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Show,Eq) 

{-3a) Define val2Int which converts a Val Type to an integer between 0 and 9. The below list may help. (2 marks)

val2Int Two = 2
val2Int Five = 5 
 -}

help = [Zero,One,Two,Three,Four,Five,Six,Seven,Eight,Nine]

val2Int::Val -> Int 
val2Int Zero = 0
val2Int One = 1
val2Int Two = 2
val2Int Three = 3
val2Int Four = 4
val2Int Five = 5
val2Int Six = 6
val2Int Seven = 7
val2Int Eight = 8
val2Int Nine = 9

{-3b) Define the type synonym Number which represents an integer as a list of Vals. (1 mark) -}

type Number = [Val]

{-3c) Using the type Number define convert which maps a Number to an Int. You should treat the first element of the list as the most significant number and the last value as the least significant. You should use val2Int to help.(3 marks)

convert [Two,Zero,Two,Zero] == 2020 
convert [Two,Six,Zero] = 260
also, convert help = 123456789 -- note that it is fine to drop leading zeros 
 -}

--uncomment once Number is defined, if you can't solve 5a) replace Number with [Val]
convert::Number -> Int 
convert = foldl (\acc num -> 10*acc + val2Int num) 0


{-4) Consider the following type -}

data OddType a = Single a (OddType a) |Tuple (a,a) (OddType a) | Triple (a,a,a) (OddType a) | Done
 deriving (Show,Eq)

 {-4a) Define hasTrip which searches an element of OddType returning True if it contains a Triple and False otherwise. You should consider pattern matching over each constructor of OddType. (2 marks)
 
 hasTrip (Single 5 (Tuple (5,5) Done)) == False 
 hasTrip (Single 4 (Triple (4,4,4) Done)) == True-}

hasTrip::OddType a -> Bool
hasTrip Done = False
hasTrip (Single a as) = hasTrip as
hasTrip (Tuple (a,b) as) = hasTrip as
hasTrip (Triple (a,b,c) as) = True

{-4b)Define findTuples which takes an element of the OddType and returns an Oddtype consisting of only the tuples from the original value. (3 marks)

findTuples (Single 1 (Tuple (2,3) (Single 4 (Tuple (4,5) Done)))) == (Tuple (2,3) (Tuple (4,5) Done)) 
findTuples (Single 1 (Single 2 (Single 3 (Single 4 Done)))) == Done  

 -}

findTuples::OddType a  -> OddType a 
findTuples Done = Done
findTuples (Single _ as) = findTuples as
findTuples (Triple (_,_,_) as) = findTuples as
findTuples (Tuple (a,n) as) = Tuple (a,n) (findTuples as)

{-4c) Define toList which maps an OddType to it's corresponding list, you should respect the original placement of values. (3 marks)

toList (Single 1 (Tuple (2,3) (Single 4 (Tuple (5,6) Done)))) == [1,2,3,4,5,6] 
toList Done == []
-}

toList::OddType a -> [a] 
toList Done = []
toList (Single a as) = [a] ++ toList as
toList (Tuple (a,b) as) = [a, b] ++ toList as
toList (Triple (a,b,c) as) = [a, b, c] ++ toList as


{-4d) Provide a functor instance for OddType. (4 marks)
##No test cases as there are many possible defintions. Just make sure fmap works!##
-}

instance Functor OddType where
  fmap f Done = Done
  fmap f (Single a as) = Single (f a) (fmap f as)
  fmap f (Tuple (a,b) as) = Tuple ((f a),(f b)) (fmap f as)
  fmap f (Triple (a,b,c) as) = Triple ((f a),(f b),(f c)) (fmap f as)

{-5a) Define mayAdd which adds two Maybe Int values together. Your code should return Nothing in the case where either value is Nothing. (1 mark)

mayAdd Nothing (Just 10) == Nothing 
mayAdd (Just 20) (Just 0) == 20 
 -}

mayAdd::Maybe Int -> Maybe Int -> Maybe Int 
mayAdd _ Nothing = Nothing
mayAdd Nothing _ = Nothing
mayAdd (Just x) (Just y) = Just (x + y)

{-5b) Define  mayFoldr which performs a right fold across a list of Maybe values, you may not use functions from the Foldable typeclass. (3 marks)

mayFoldr [(Just 5), (Just 4), (Just 3), (Just 2), (Just 1)] mayAdd (Just 0) == Just 15
mayFoldr [(Just 5), (Just 4),Nothing, (Just 2), (Just 1)] mayAdd (Just 0) == Nothing 
-}

mayFoldr::[Maybe a] -> (Maybe a -> Maybe b -> Maybe b) -> Maybe b -> Maybe b
mayFoldr (a:as) f b = mayFoldr as f (f a b)
mayFoldr [] f b = b

{-5c) The Either type holds a Left or Right value which can be of different types. Using Either define myIf which takes a bool, two tuples (f::a->b, x::a) and (g::c->d,y::c) using Either, to emulate an if statement - if bool then f a else g c. (2 marks)

myIf True ((+1),5) (tail,"Mytest") == Left 6
myIf False ((+1),5) (tail,"Mytest") == Right "ytest"
 -}

myIf::Bool -> (a -> b, a) -> (c -> d, c) -> Either b d 
myIf True (f,a) (g,c) = Left (f a)
myIf False (f,a) (g,c) = Right (g c)