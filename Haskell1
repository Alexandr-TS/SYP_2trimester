comb n m = 
    fact n `div` fact m `div` fact (n - m) where
    fact x = 
        if x <= 1 
            then 1
        else 
            x * fact (x - 1) 

cut2 list =
    if length list == 0 then []
    else head list : cut2 (drop 2 list) 

gcd1 a b =
    if a == 0 
        then b
    else
        gcd1 (b `mod` a) a 

months l =
    snd (maximum t) where
        t = [(x, y) | y <- [1..12], x <- [0..length l], x == days y] where
            days month = 
                length [x | (x, y) <- l, y == month]
{- 
months [(1,12),(5,12),(5,6),(6,4),(7,4),(10,4)]
months [(1,12),(5,12),(5,12),(6,4),(7,5),(10,4),(8, 4)]
  -}

nearest l =
    snd (minimum [(z, (x, y)) | (x, y, z)<-l])
{-  
nearest [("A", "B", 10), ("A", "C", 20), ("D", "E", 7), ("F", "G", 30)]  
nearest [("A", "B", 10), ("A", "C", 20), ("D", "E", 40)]  
-}

queens l =
    length [((a, b),(c, d)) | (a, b)<-l, (c, d)<-l, (a /= c || b /= d) && 
        (abs(a - b) == abs(c - d) || a + b == c + d || a == c || b == d)] > 0 where
        abs x = if x < 0 then -x else x
{-
queens [(1,1), (6,6), (8,1), (8,2)]
queens [(1,1), (2,3), (3,5), (4,7)]
-}
