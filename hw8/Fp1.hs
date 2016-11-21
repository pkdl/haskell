head' :: [a] -> a
head' [] = undefined
head' (x:xs) = x

tail' :: [a] -> [a]
tail' [] = []
tail' (_:xs) = xs

take' :: Int -> [a] -> [a]
take' n a | n == 1 = [head a]
          | otherwise = (head a):(take' (n-1) (tail' a))

drop' :: Int -> [a] -> [a]
drop' n a = if n == 1 then tail a else drop (n-1) (tail' a)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f xs | (f (head' xs)) == True = (head' xs) : (filter' f (tail' xs))
             | (f (head' xs)) == False = filter' f (tail' xs)

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (f z x) xs

reverse' :: [a] -> [a]
reverse' a = foldl' listadd [] a

listadd :: [a] -> a -> [a]
listadd a b = b:a

concat' :: [a] -> [a] -> [a]
concat' a b = foldl' listadd b (reverse' a)

quickSort' :: Ord a => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) = concat' (quickSort' l) (x:(quickSort' r))
    where
        l = filter' (<= x) xs
        r = filter' (> x) xs