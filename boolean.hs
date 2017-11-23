infix 1 ==>
(==>) :: Bool -> Bool -> Bool
True  ==> x = x
False ==> x = True

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

valid1 :: (Bool -> Bool) -> Bool
valid1 bf = and [bf p | p <- [True, False]]

valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = and [bf p q | p <- [True, False],
                          q <- [True, False]]

valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [bf p q r | p <- [True, False],
                            q <- [True, False],
                            r <- [True, False]]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [bf p q r s | p <- [True, False],
                              q <- [True, False],
                              r <- [True, False],
                              s <- [True, False]]

invalid1 :: (Bool -> Bool) -> Bool
invalid1 bf = not (
  or [bf p | p <- [True, False]]
  )

invalid2 :: (Bool -> Bool -> Bool) -> Bool
invalid2 bf = not (
  or [bf p q | p <- [True, False],
               q <- [True, False]]
  )

invalid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
invalid3 bf = not (
  or [bf p q r | p <- [True, False],
                 q <- [True, False],
                 r <- [True, False]]
  )

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = and [bf1 p <=> bf2 p | p <- [True, False]]

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [bf1 p q <=> bf2 p q | p <- [True, False],
                                               q <- [True, False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [bf1 p q r <=> bf2 p q r | p <- [True, False],
                                                   q <- [True, False],
                                                   r <- [True, False]]


form1 :: Bool -> Bool -> Bool
form1 p q = p ==> (q ==> p)

form2 :: Bool -> Bool -> Bool
form2 p q = (p ==> q) ==> p

formula :: Bool -> Bool -> Bool
formula p q = ((not p) && (p ==> q) <=> not (q && (not p)))

formula3 :: Bool -> Bool -> Bool
formula3 p q = p

formula4 :: Bool -> Bool -> Bool
formula4 p q = (p <+> q) <+> q

formula5 :: Bool -> Bool -> Bool
formula5 p q = p <=> ((p <+> q) <+> q)


-- chapter 2.8

every, some :: [a] -> (a -> Bool) -> Bool
every xs p = all p xs
some xs p = any p xs


uniqe :: (a -> Bool) -> [a] -> Bool
uniqe p x = length (filter p x) == 1

parity :: [Bool] -> Bool
parity x = even (length (filter (== True) x))

parity' :: [Bool] -> Bool 
parity' [] = True
parity' (x:xs) = x /= (parity xs)

evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p x = even (length (filter p x ))

evenNR' :: (a -> Bool) -> [a] -> Bool
evenNR' p = parity . map p


-- wrong (def of p57 Expressing Uniqeness) because the uniqueness of a number is defined by its value AND its position in the list!!
uniq :: (Eq a) => (a -> Bool) -> [a] -> Bool 
uniq p xs = some xs p && some xs (\x -> every xs (\ z -> (p z) ==> (x == z)))
