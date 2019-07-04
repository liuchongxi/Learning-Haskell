{-
reference;
Learn list comprehension: https://wiki.haskell.org/List_comprehension
used is_prime function: http://www.sfu.ca/~tjd/383summer2019/haskell_functions_lhs.html
-}

{-(1 mark) The snoc x lst function returns a new list that is the same as lst, except x has been added to the end of it. It has this signature:

snoc :: a -> [a] -> [a]
For example, snoc 5 [1,2,3] is [1,2,3,5], and snoc 's' "cat" is "cats".

Implement snoc using only basic recursion. Do not use ++ or reverse or any other such high-level functions.-}

--Q1
snoc :: a -> [a] -> [a]
snoc x []           = [x]
snoc x (first:rest) = first : (snoc x rest)


{-(1 mark) Write your own version of the Haskell append operator ++ with this signature:

myappend :: [a] -> [a] -> [a]
Of course, don’t use functions like ++ or concat in your answer.-}

--Q2
myappend :: [a] -> [a] -> [a]
--myappend [] b  = b
--myappend a b   = (head a) : (myappend (tail a) b)
myappend a []           = a
myappend a (first:rest) = myappend (snoc first a) rest


{-(1 mark) Write your own version of reverse with this signature:

myreverse :: [a] -> [a]
Don’t use any non-trivial functions in your answer unless you write those functions yourself.-}

--Q3
myreverse :: [a] -> [a]
myreverse []           = []
myreverse (first:rest) = snoc first (myreverse rest)

{-(1 mark) Write a function called count_emirps n that returns the number of emirps less than, or equal to, n.

An emirp is a prime number that is a different prime when its digits are reversed. For example, 107 is an emirp because 107 is prime, and its reverse, 701, is a different prime. However, 7 and 101 are not emirps because while their reverses are primes, they are not different primes.

The first few emirps are: 13, 17, 31, 37, 71, 73, ….

count_emirps has this signature:

count_emirps :: Int -> Int
For example, count_emirps 100 returns 8, and count_emirps 1000 returns 36. If n is less than 13, count_emirps n returns 0. -}

--Q4
count_emirps :: Int -> Int
count_emirps n
             | n<0       = 0
             | otherwise = length (filter is_emirps [1..n])

is_emirps :: Int -> Bool
is_emirps n = reverse_n /= n && is_prime n && is_prime reverse_n
             where reverse_n = reverse_int n

reverse_int :: Int -> Int
reverse_int n = read (myreverse (show n)) :: Int

is_prime :: Int -> Bool
is_prime n
         | n < 2     = False
         | otherwise = (smallest_divisor n) == n

smallest_divisor :: Int -> Int
smallest_divisor n
                 | n < 0     = error "n must be >= 0"
                 | n == 0    = 0
                 | n == 1    = 1
                 | otherwise = head (dropWhile (\x -> n `mod` x /= 0) [2..n])

{-(1 mark) Write a function called biggest_sum that takes a list of one, or more, integer lists as input, and returns the list with the greatest sum. It has this signature:

biggest_sum :: [[Int]] -> [Int]
For example, biggest_sum [[2,5], [-1,3,4], [2]] returns [2,5].

You can assume the list passed to biggest_sum is non-empty.

If one, or more more, lists are tied for the biggest sum, then return the first one. -}
--Q5
biggest_sum :: [[Int]] -> [Int]
biggest_sum lst
             |length lst == 1         = first
             |sum first >= sum second = first
             |otherwise               = second
             where first   = head lst
                   second  = biggest_sum (tail lst)

{-(1 mark) Write a function called greatest, which has the following signature:

greatest :: (a -> Int) -> [a] -> a
greatest f seq returns the item in seq that maximizes function f. For example:

> greatest sum [[2,5], [-1,3,4], [2]]
[2,5]

> greatest length ["the", "quick", "brown", "fox"]
"quick"

> greatest id [51,32,3]
51
If more than one item maximizes f, then greatest f returns the first one.-}
--Q6
greatest :: (a -> Int) -> [a] -> a
greatest f lst
             |length lst == 1        = first
             |f first >= f second    = first
             |otherwise              = second
             where first   = head lst
                   second  = greatest f (tail lst)

{-(1 mark) Write a function called is_bit x that returns True when x is 0 or 1, and False otherwise.

Assume x is of type Int, and the type of the returned value is Bool.

Include the most general type signature.-}
--Q7
is_bit :: Int -> Bool
is_bit x
       | x == 0 || x == 1 = True
       | otherwise        = False

{-(1 mark) Write a function called flip_bit x that returns 1 if x is 0, and 0 if x is 1. If x is not a bit, then call error msg, where msg is a helpful error message string.

Assume x is of type Int, and the type of the returned value is also Int.

Include the most general type signature.-}
--Q8
flip_bit :: Int -> Int
flip_bit x
         | not (is_bit x) = error "input is not a bit"
         | x == 1         = 0
         | otherwise      = 1

{-In each of the following functions, x is a list of Int values, and the returned value has type Bool. Include the most general type signature for each function.

(1 mark) Write a function called is_bit_seq1 x that returns True if x is the empty list, or if it contains only bits (as determined by is_bit). It should return False otherwise.
Use recursion and guarded commands in your solution.-}
--Q9.a
is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 seq
               | null seq  = True
               | otherwise = is_bit first && is_bit_seq1 rest
               where first = head seq
                     rest  = tail seq


{-(1 mark) Re-do the previous question, except this time name the function is_bit_seq2 x, and use recursion and at least one if-then-else expression in your solution. Don’t use any guarded commands.-}
--Q9.b
is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 seq = if null seq
                  then True
                  else is_bit first && is_bit_seq2 rest
                  where first = head seq
                        rest  = tail seq

{-(1 mark) Re-do the previous question, except this time name the function is_bit_seq3 x, and don’t use recursion, guarded commands, or if- then-else in your solution. Instead, use a higher-order function to calculate the answer in one expression.-}
--Q9.c
is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 seq = all is_bit seq

{-In each of the following functions, x is a list of Int values, and the type of the returned value is also a list of Int. Include the most general type signature for each function.

(1 mark) Write a function called invert_bits1 x that returns a sequence of bits that is the same as x, except 0s become 1s and 1s become 0s. For example, invert_bits1 [0,1,1,0] returns [1,0,0,1].
Use basic recursion in your solution.-}
--Q10.a
invert_bits1 :: [Int] -> [Int]
invert_bits1 seq
             | null seq  = []
             | not (is_bit first) = error "input is not a sequence of bits"
             | otherwise =  flip_bit first : invert_bits1 rest
             where first = head seq
                   rest  = tail seq

{-(1 mark) Re-do the previous question, but name the function invert_bits2 x, and implement it using the map function (and no recursion).-}
--Q10.b
invert_bits2 :: [Int] -> [Int]
invert_bits2 seq = map flip_bit seq

{-(1 mark) Re-do the previous question, but name the function invert_bits3 x, and implement it using a list comprehension (and no recursion, and no map function).-}
--Q10.c
invert_bits3 :: [Int] -> [Int]
invert_bits3 []  = []
invert_bits3 seq = [ flip_bit n | n <- seq]

{-(1 mark) Write a function called bit_count x that returns a pair of values indicating the number of 0s and 1s in x. For example, bit_count [1,1,0,1] returns the pair (1, 3), meaning there is one 0 and three 1s in the list.

Assume x is a list of Int values, and only contains bits. The type of the returned value is (Int, Int).

Include the most general type signature.-}
--Q11
-- bit_count [0,1,1,1,1, 0,9]
-- => (2,4) -- May need error check here
bit_count :: [Int] -> (Int, Int)
bit_count seq = ((count 0 seq), (count 1 seq))

count :: Int -> [Int] -> Int
count x seq
         | null seq   = 0
         | x == first = 1 + count x rest
         | otherwise  = count x rest
         where first = head seq
               rest  = tail seq

{-(1 mark) Write a function called all_basic_bit_seqs n that returns a list of all bit sequences of length n. The order of the sequences doesn’t matter. If n is less than 1, then return an empty list.

Assume n is an Int, and the returned value is a list of Int lists.

Include the most general type signature.-}
--Q12
all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n
                   | n < 1     = []
                   | n == 1    = [[0],[1]]
                   | otherwise = myappend (map (myappend [0]) (all_basic_bit_seqs (n-1)))
                                          (map (myappend [1]) (all_basic_bit_seqs (n-1)))


data List a = Empty | Cons a (List a)
   deriving Show

{-(1 mark) Implement toList :: [a] -> List a, which converts a regular Haskell list to a List a. For example:

> toList []
Empty

> toList [2, 7, 4]
Cons 2 (Cons 7 (Cons 4 Empty))

> toList "apple"
Cons 'a' (Cons 'p' (Cons 'p' (Cons 'l' (Cons 'e' Empty))))-}
--Q13
toList :: [a] -> List a
toList []     = Empty
toList (x:xs) = Cons (x) (toList xs)

{-(1 mark) Implement toHaskellList :: List a -> [a], which converts a List a to a regular Haskell list. For example:

> toHaskellList Empty
[]

> toHaskellList (Cons 2 (Cons 7 (Cons 4 Empty)))
[2,7,4]

> toHaskellList (Cons "cat" (Cons "bat" (Cons "rat" Empty)))
["cat","bat","rat"]-}
--Q14
toHaskellList :: List a -> [a]
toHaskellList Empty            = []
toHaskellList (Cons head rest) = [head] ++ (toHaskellList rest)

{-For the following questions, don’t use toList or toHaskellList in your implementations. Only use them for testing and debugging. Stick to basic recursion and Haskell prelude functions for your solution code.

Make sure to give the most general type signatures for each of the functions.

(1 mark) Implement append A B, that returns a new List a that consists of all the elements of A followed by all the elements of B. In other words, it does for List a what ++ does for regular Haskell lists. For example:

> append (Cons 1 (Cons 2 (Cons 3 Empty))) (Cons 7 (Cons 8 Empty))
Cons 1 (Cons 2 (Cons 3 (Cons 7 (Cons 8 Empty))))-}
--Q15
append :: List a -> List a -> List a
append Empty b = b
append (Cons head rest) b = Cons head (append rest b)

{-(1 mark) Implement the function removeAll f L that returns a List a that is the same as L but all items satisfying f (i.e. for which f returns True) have been removed. f is a predicate function of type a -> Bool and L has type List a.

For example:

> removeAll even Empty
Empty

> removeAll (\x -> x == 'b') (Cons 'b' (Cons 'u' (Cons 'b' Empty)))
Cons 'u' Empty

> removeAll even (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))
Cons 1 (Cons 3 Empty)

> removeAll odd (Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty))))
Cons 2 (Cons 4 Empty)-}
--Q16
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty            = Empty
removeAll f (Cons head rest) = if (f head) then (removeAll f rest) else Cons head (removeAll f rest)

{-(1 mark) Implement sort L, where L has type List a, that returns a new List a that is a sorted version of L (in ascending order). Use either quicksort or mergesort.

It must have this type signature:

sort :: Ord a => List a -> List a
Ord a => means that the type a must be usable with comparisons functions such as <, <=, ==, etc.

For example:

> sort Empty
Empty

> sort (Cons 'c' (Cons 'a' (Cons 'r' (Cons 't' Empty))))
Cons 'a' (Cons 'c' (Cons 'r' (Cons 't' Empty)))-}
--Q17
{-
-- solution 1
sort1 :: Ord a => List a -> List a
sort1 Empty       = Empty
sort1 lst         = toList (quicksort (toHaskellList lst))

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = smalls ++ [x] ++ bigs
                   where smalls = quicksort [n | n <- xs, n <= x]
                         bigs   = quicksort [n | n <- xs, n > x] -}

sort :: Ord a => List a -> List a
sort Empty            = Empty
sort (Cons head rest) = append smalls (Cons head bigs)
                         where smalls = sort (removeAll (\x -> x > head) rest)
                               bigs   = sort (removeAll (\x -> x <= head) rest)


