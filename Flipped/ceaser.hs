-- Cryptography examples
-- URL: http://www.macs.hw.ac.uk/~hwloidl/Courses/F21CN/index.html
-- This file: http://www.macs.hw.ac.uk/~hwloidl/Courses/F21CN/Labs/caesar.hs
-- Exercises for the Cryptography part of the above course F21CN
--
-- Run the exercises on the Linux lab machines after doing the installation:
--  # wget http://www.macs.hw.ac.uk/~hwloidl/Courses/F21CN/Labs/f21cn.sh
--  # source f21cn.sh
--  # install_ghc_pkgs
-- Installation is successful, if you see these lines at the end:
--  You should now find 'QuickCheck' and 'HaskellForMaths' in the list below:
--     QuickCheck-2.4.1.1
--     HaskellForMaths-0.1.9
-- After that, start the GHCi interpreter like this:
--  # ghci -package HaskellForMaths -package QuickCheck caesar.hs
-- Now, go through this file and test code snippets, by cut-and-paste of lines
-- starting with (#> represents the GHCi prompt):
-- #> 
-- eg,
-- #> test
-----------------------------------------------------------------------------

module Caesar where

import Data.Char
import Math.Algebra.Field.Base
import Data.List
import Test.QuickCheck

-----------------------------------------------------------------------------
-- Caesar Cipher Example
-- from Hutton "Programming in Haskell", p42ff
-- The code below implements a simple (rotation-bsaed) Caesar cipher.
-- It is very easy to crack, which is done by the function crack.
-- See the exercise below
-----------------------------------------------------------------------------

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- shift a character c by n slots to the right
shift :: Int -> Char -> Char
shift n c | isLower c = int2let (((let2int c) + n) `mod` 26)
          | otherwise = c

-- top-level string encoding function
encode :: Int -> String -> String
encode n cs = [ shift n c | c <- cs ]

-- table of frequencies of letters 'a'..'z'
table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1,  6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m)*100

-- compute frequencies of letters 'a'..'z' in a given string
freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a'..'z'] ]
           where n = lowers cs

-- chi-square function for computing distance between 2 frequency lists
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

-- rotate a list by n slots to the left; take, drop are Prelude functions
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- the number of lower case letters in a given string
lowers :: String -> Int
lowers cs = length [ c | c <- cs, isLower c]

-- count the number of occurrences of c in cs
count :: Char -> String -> Int
count c cs = length [ c' | c' <- cs, c==c']

-- find list of positions of x in the list xs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i' | (x', i') <- zip xs [0..n], x==x' ]
                 where n = length xs - 1

-- top-level decoding function
crack :: String -> String
crack cs = encode (-factor) cs
           where factor = head (positions (minimum chitab) chitab)
                 chitab = [ chisqr (rotate n table') table | n <- [0..25] ]
                 table' = freqs cs

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Exercise:
-- encrypting a simple text string
-- #> let s1 = "a completely random text string"
-- #> let c1 = encode 3 s1
-- #> c1
-- now, let's try to crack it
-- #> let d1 = crack c1
-- Q: is it the same as s1?
-- #> d1
-- there are no 'e's in the string below
-- Q: do you believe we can still crack this string?
-- #> let s2 = "unusal random string"
-- #> let c2 = encode 7 s2
-- #> c2
-- #> let d2 = crack c2
-- #> d2
-- Now, let's try the string below
-- #> let s3 = "an apple fell off"
-- #> let c3 = encode 13 s3
-- #> c3
-- #> let d3 = crack c3
-- #> d3
-- Q: why do you think cracking failed in the above example?
-- Test your hypothesis by devising another input, encode and try to crack it
-- Now, let's try some randomly generated text as input
-- We read in words from a dictionary (in words.txt)
-- #> str <- readFile "words.txt"
-- #> let words = lines str
-- Now we define a function to build text by selecting words from the list
-- #> let mkStr = \ (n:xs) -> map toLower (foldl1 (\ x ys -> x ++ " " ++ ys) (map (\ i -> words!!i) (take n (filter (<(length words)) (nub xs)))))
-- Test this by generating text consisting of 4 (first number in the list) words
-- #> mkStr [4,23349,98422,52321,1942,453,8777]
-- and try to crack it
-- #> crack (encode 6 (mkStr [4,23349,98422,52321,1942,453,8777]))
-- We can use this function together with quickcheck to test random texts, containing 5 to 9 words from the dictionary
-- #> quickCheck ( \ ys -> length ys > 66 ==> let (n:xs) = ys in   (n>5 && n<9 ) ==>  let str = mkStr (n:xs) in crack (encode 13 str) == str  )
-- It will fail for a degenerate case, such as picking [16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
-- Check the string, which triggers the failed check like this
-- #> mkStr [16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
-- You can also use quickcheck to encode some random string str (btw 10 and 50 characters long)
-- and then trying to crack the encoded string; it will report the first failed crack
-- #> quickCheck (\str -> length str < 10 || length str > 50 || crack (encode 7 str) == str)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- Vernam cipher (using a sequence of shift values as key)
vernam = zipWith shift

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Exercise:
-- encrypting a simple text string using a vernam cipher
-- #> vernam [1..] "some message"
-- "tqpi slabkrq"
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- -----------------------------------------------------------------------------
-- Modular arithmetic using Math.Algebra.Field.Base
-- -----------------------------------------------------------------------------

showtab' n str [] = str
showtab' n str ([]:xss) = showtab' (n+1) (str++"\n"++" "++(if not (null xss) then show (n+1) ++ ":" else "")) xss
showtab' n str ((x:xs):xss) = showtab' n (str++" "++(show x)) (xs:xss)  

-- show a multiplication table
showtab n = showtab' 0 ("    "++(concat (intersperse " " (map show [0..n-1])))++"\n 0:") (mkTab n) 

-- build a multiplication table
mkTab n = [ [ i*j `mod` n | j <- [0..n-1] ] | i <- [0..n-1] ] 

-- Example:
-- #> putStrLn (showtab 5)

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Exercise:
-- In the examples below, try to answer each question
-- yourself before you execute the line following it.
-- multiplication in the modular domain, mod 5
-- (2 :: F5) means, 2 is a number in the modular domain mod 5
-- Q: what's the result?
-- #> (2 :: F5)*(3 :: F5)
-- try more examples, using +, -, *
-- use variables like this:
-- #> let x = (2 :: F5)
-- #> x*x*x
-- the notation x^n means x to the power of n; 
-- Q: can you calculate the result below, using pen and paper,
--    or even in your head? Explain how you can compute it.
-- #> x^10
-- here is the multiplication table, mod 5
-- #> putStrLn (showtab 5)
-- exponentiation in the modular domain, mod 5
-- Q: Using the table above, what's the result?
-- #> (2 :: F5)^3
-- inverse mod 5
-- Q: how do you use the multiplication table to find an inverse? eg:
-- #> recip (3::F5)
-- how to you read-off the inverse from the table
-- #> putStrLn (showtab 5)
-- let's test it
-- #> recip (4::F5)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- -----------------------------------------------------------------------------
-- RSA encryption
-- -----------------------------------------------------------------------------
-- Auxiliary functions

-- modular inverse, i.e. 
mod_inv n e = head [ x | x <-[1..n-1], e*x `mod` n == 1 ]

-- (naive) Euler totient function
euler :: Integer -> Integer
euler n = fromIntegral (length [ x | x <- [(1::Integer)..n], gcd x n == 1])

-- relatively prime
rel_prime :: Integer -> Integer -> Bool
rel_prime x y = gcd x y == 1

-- all prime numbers (infinite list)
primes :: [Integer]
primes = 2:[ p | p <- [3,5..], isprime primes p ] 
        where isprime (x:xs) p = if p>=x^2 then not (p `mod` x == 0) && isprime xs p else True
-- test
is_prime :: Integer -> Bool
is_prime n = n == head (dropWhile (<n) primes)

-- naive integer factorisation
factors :: Integer -> [Integer]
factors x = [ p | p <- takeWhile (<x) primes, x `mod` p == 0 ]

pick_e (p,q) = head [ x | x <- primes, not (x `elem` ps) ]
               where ps = factors ((p-1)*(q-1))

-- -----------------------------------------------------------------------------
-- RSA encryption
-- -----------------------------------------------------------------------------
-- main functions

-- RSA encryption is simple: you just compute the exponential
encrypt (e,n) m = m^e `mod` n
-- RSA decryption is almost the same:
decrypt (d,n) m = m^d `mod` n

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- RSA Exercise::
-- we first need to pick 2 prime numbers; to do that we
-- select the 7-th and 8-th element of the infinite list
-- prime numbes, 'primes'
-- #> let p = primes!!7
-- #> let q = primes!!8
-- #> let n = p*q
-- #> let phi = (p-1)*(q-1)
-- we need to select a (public) encryption key 'e', which is not
-- a factor of the 'phi' computed above
-- #> factors phi
-- we see that the value 7 is a valid choice
-- #> let e = 7
-- we double check that it is a valid choice by computing the gcd
-- #> gcd e phi
-- the result must be 1
-- now, we can compute the (secret) decryption key as follows:
-- #> let d = mod_inv phi e
-- we check that 'd' works as a decryption key, by computing:
-- #> e*d `mod` phi
-- the result must be 1
-- now, that we have both encryption and decryption keys, we can encode 'm'
-- #> let enc m = m^e `mod` n
-- and decode the encrypted message 'c' like this
-- #> let dec c = c^d `mod` n
-- let's test this, using the integer 99 as message
-- #> let m=99
-- we encode the message like this
-- #> let c = enc m
-- #> c
-- and decode it like this
-- #> dec c
-- the result should be the original message, 99
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- Exercise: testing properties
-- The following property states correctness of encryption:
-- for any random integer m, encryting and then decrypting 
-- gives you the original message:
-- #> quickCheck (\ m -> dec (enc (abs m)) == (abs m))
-- This property states that the Euler totient of a prime m is m-1
-- #> quickCheck (\ m -> m<0 || not (is_prime m) || euler m == m-1)
-- This is the Euler Theorem:
-- #> quickCheck (\ m n -> m<2 || n<2 || not (gcd m n == 1) || m^(euler n) `mod` n == 1)
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++


test = do
 let p = primes!!7
 putStrLn $ "We pick p = "++(show p)
 let q = primes!!8
 putStrLn $ "We pick q = "++(show q)
 let n = p*q
 putStrLn $ "We have n = p*q = "++(show n)
 let phi = (p-1)*(q-1)
 putStrLn $ "We have phi = (p-1)*(q-1) = "++(show phi)
 putStrLn $ "Factors of "++(show phi)++" = "++(show (factors phi))
 let ps = factors ((p-1)*(q-1))
 let e = head [ x | x <- primes, not (x `elem` ps) ]
 putStrLn $ "We pick e (relatively prime to phi) = "++(show e)
 putStrLn $ "gcd e phi (expect 1) = "++(show (gcd e phi))
 let d = mod_inv phi e
 putStrLn $ "e*d `mod` phi (expect 1) = " ++ (show ( e*d `mod` phi))
 let enc m = m^e `mod` n
 let dec c = c^d `mod` n
 let m = 99
 putStrLn $ "Message = "++(show m)
 let c = enc m
 putStrLn $ "Ciphertext = "++(show c)
 let m' = dec c
 putStrLn $ "Decrypted ciphertext = "++(show m')

-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- a test wrapper; run it like this
-- #> test
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++