divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ld :: Integer -> Integer
ld n = ldf 2 n 

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k 
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n 

prime0 :: Integer -> Bool
prime0 n | n < 1     = error "not a positive integer"
         | n == 1    = False
         | otherwise = ld n == n

ldp :: Integer -> Integer
ldp n = ldpf primes1 n

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes1 :: [Integer]
primes1 = 2 : 3 : 5 : 7 : 11 : 13 : 17 : 19 : filter prime [23..]

prime :: Integer -> Bool 
prime n | n < 1 = error "not a positive integer"
        | n == 1 = False 
        | otherwise = ldp n == n


minInt :: [Int] -> Int
minInt x = listComperator x min

maxInt :: [Int] -> Int
maxInt x = listComperator x max

listComperator :: [a] -> (a -> a -> a) -> a
listComperator [] f = error "empty list"
listComperator [x] f = x 
listComperator (x:xs) f = f x (listComperator xs f)


rmfirst :: a -> (a -> a -> Bool) -> [a] -> [a]
rmfirst _ _ [] = []
rmfirst y f (x:xs) | f y x = xs
                   | otherwise = x : rmfirst y f xs

firstString :: String -> [String] -> String
firstString x [] = x
firstString y (x:xs) | y < x = firstString y xs
                     | otherwise = firstString x xs

minString :: [String] -> String
minString x = listComperator x min 

maxString :: [String] -> String
maxString x = listComperator x max


srtString :: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (rmfirst m (==) xs)) where m = minString xs

srtInt :: [Int] -> [Int]
srtInt [] = []
srtInt xs = m : (srtInt (rmfirst m (==) xs)) where m = minInt xs 

average :: [Int] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum' xs) / fromIntegral (length' xs)

sum' :: [Int] -> Int 
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int 
length' [] = 0 
length' (x:xs) = 1 + length' xs

count :: Char -> String -> Int
count _ [] = 0 
count x (y:ys) | x == y = 1 + count x ys 
               | otherwise = count x ys



blowup :: String -> String 
blowup [] = []
blowup x = replaceAll 0 x

multiply :: Char -> Int -> String
multiply x 0 = ""
multiply x y = [x] ++ (multiply x (y-1))

replaceAll :: Int -> String -> String 
replaceAll x [] = []
replaceAll x (y:ys) = (multiply y (x+1)) ++ (replaceAll (x+1) ys)



prefix :: String -> String -> Bool 
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

substring :: String -> String -> Bool 
substring [] ys = True
substring x (y:ys) | length x > length (y:ys) = False
                   | otherwise = (prefix x (y:ys)) || substring x ys

substring' :: String -> String -> Bool
substring' xs (y:ys) | prefix xs (y:ys) = True
                     | length xs > length (y:ys) = False
                     | otherwise = substring' xs ys 

factors :: Integer -> [Integer]
factors n | n < 1  = error "argument not positive"
          | n == 1 = []
          | otherwise = p : factors (div n p) where p = ldp n

lengths :: [[a]] -> [Int]
lengths x = map length x

sumlengths :: [[a]] -> Int
sumlengths x = sum (lengths x)

primes0 :: [Integer]
primes0 = filter prime0 [2..]

factorial :: Integer -> Integer
factorial n | n<1       = error "argument not positive"
            | otherwise = product [1..n]

squares :: Int -> [Int]
squares n | n < 0     = error "argument noch positive"
          | otherwise = take n listsquares  where listsquares = [x^2 | x <- [1..]]

fib :: Int -> [Int]
fib x = take x fibList
  where fibList = 1 : 1 : [ x + y | (x,y) <- zip fibList (tail fibList)]


triangles :: Integer -> [(Integer, Integer, Integer)]
triangles n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2+b^2 == c^2, (a <= b) ]


