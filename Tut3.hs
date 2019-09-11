-- Efficient Recursion and Higher-Order Functions.
-- Please submit your Tut3 solution by 20 Sept 6pm
-- on IVLE for participation mark

import           Data.Array
import           Debug.Trace (trace)

{-
  stack ghci
  Prelude> :l Tut3.hs
-}

{-
    Q1: You have been asked to implement a list of factorials.
	A naive way of implementing it is given below. The naive
	algorithm has O(n^2) complexity.

    Can you write a more efficient tupled recursive method, call
    it factlist_tup, to do this in O(n) time?

-}

fact :: (Eq t, Num t) => t -> t
fact 0 = 1
fact n = n * fact (n-1)

factlist :: (Eq t, Num t) => t -> [t]
factlist 0 = [1]
factlist n = fact n : factlist (n-1)

factlist_tup :: (Eq t,Num t) => t -> [t]
--factlist_tup n = error "To be implemented using tupled recursion and Integer"
factlist_tup 0 = [1]
factlist_tup n =
  let res = factlist_tup (n-1)
  in
    (head res) * n : factlist_tup (n-1)


{-
    Q2.
    (a)  Guess the types for each of the functions below.
    (b)  Write two examples of each of the functions below.
    (c)  Write a tail recursive version of (|>>|), calling it |>>>|.
         Is it helpful for this function to be tail-recursive?
         You may use the rev method that can help reverse a list
            rev :: [t] -> [t]

-}

(|>>|) :: (t -> a) -> [t] -> [a]
(|>>|) _ []     = []
(|>>|) f (x:ys) = f x : f |>>| ys

fold_right :: (t1 -> t2 -> t2) -> [t1] -> t2 -> t2
fold_right _ [] a     = a
fold_right f (x:ys) a = f x (fold_right f ys a)

map2 :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
map2 _ [] []         = []
map2 _ _ []          = error "Length of two lists are not equal"
map2 _ [] _          = error "Length of two lists are not equal"
map2 f (x:xs) (y:ys) = f x y : map2 f xs ys


(|>>>|) :: (a -> b) -> [a] -> [b]
(|>>>|) f xs =
    let aux [] zx     = zx
        aux (x:ys) zx = aux ys (f x :zx)
    in
        aux xs []


{-
    Q3. Use fold_right to implement each of
    the following functions.

-}

len :: Num p => [a] -> p
len [] = 0
len (x:xs) = 1 + (len xs)

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

sumlen :: (Num a, Num b) => [a] -> (a, b)
sumlen xs = fold_right (\x (sum, len) -> (sum + x, len + 1)) xs (0,0)

--sumlen [] = (0,0)
--sumlen (x:xs) = let (a,b) = sumlen xs in (x+a,1+b)

app :: [a] -> [a] -> [a]
--app xs ys = fold_right (\x acc -> x : acc) xs ys
app = fold_right(:)

--app [] ys = ys
--app (x:xs) ys = x:(app xs ys)

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f [] = []
filter2 f (x:xs) =
  if f x then x:(filter2 f xs)
  else filter f xs


{-
    Q4. Can you rewrite dropWhile2 below to use fold_right.
    This is a bit more challenging

    Your first attempt may involve:

    dropWhile2 f xs =
      fold_right (\x r -> if f x then r else ?) xs []

   However, the remaining list "xs" is not avaialble
   in r. Hint: You need to think about how this may be added.

-}

dropWhile2 :: (a -> Bool) -> [a] -> [a]
--dropWhile2 f [] = []
--dropWhile2 f ys@(x:xs) =
--  if f x then dropWhile2 f xs
--  else ys

dropWhile2 pred xs = fst (fold_right (\x (res, tl) ->
  if pred x then (res, x:tl) else (x:tl, x:tl) ) xs ([], []))

--dropWhile2 (\x -> mod x 2 == 0) [2,3,4,5]


{-
    Q5. Can you write a function 'for_all' that takes a function 'f' and a list '[x1, x2, ...]' as parameter
        and return true if and only if 'f x1 & f x2 & ... is true'
    Write this as a recursive function.
    After that, implement it using fold_right.
-}

for_all :: (a -> Bool) -> [a] -> Bool
for_all f [] = True
for_all f (x:xs) = f x && for_all f xs


{-
Q6. Implement a higher-order function that would apply
  a given function n-times:
  iter n f x = f (f ...(f x) ..)
  f . f . f . f $ x

Can you re-implement this using fold_right together
with the help of other auxiliary functions?

-}

iter :: Int -> (a->a) -> a -> a
--iter n f x = fold_right (\_ acc -> f acc) [1..n]
--iter n f x = fold_right (\_  -> f ) [1..n]
iter n f x = fold_right (const f ) [1..n]



{-
Q7. Implement iter in terms of
    of the composition operator (.)
    where f is applied n times

	What is the inferred type of iter2

-}
--Monoid
iter2 n f = fold_right (\_ acc -> f . acc ) [1..n] id
--(f.(f.(f.f)))x

iter3 n f  = fold_left (\_ acc -> f. acc) [1..n] id
--(((f.f).f).f)x

--compose f g x = f (g x)

{-
Q8. Consider the function that applies the map function twice.
    Rewrite this function using only a single map.
-}

double_map f g xs = map f (map g xs)
single_map f g xs = map (f . g) xs

main = putStrLn  "Tutorial 3"
