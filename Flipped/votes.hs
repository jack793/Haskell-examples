
-- Exercise 7.7Voting algorithms
-- Graham Hutton. “Programming in Haskell”

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

rmdups :: Eq a => [a] -> [a]
rmdups []= []
rmdups (x:xs) = x : rmdups( filter(/=x) xs)

result :: Ord a => [a] -> [(Int,a)]
result vs = sort [(count v vs, v) | v <- rmdups vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- ALTERNATIVE SOLUTION

ballots ::   [[String]]
ballots = [["Red", "Green"],
["Blue"],
["Green", "Red", "Blue"],
["Blue", "Green", "Red"],
["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner’ :: Ord a => [[a]] -> a
winner’ bs = case rank (rmempty bs) of
[c]-> c
(c:cs) -> winner’ (elim c bs)
