add a b = a + b

minus :: Num a => a -> a -> a
minus a b = a - b

myDrop :: Int -> [a] -> [a]
myDrop n xs =
    if n <= 0 || null xs
    then xs
    else myDrop (n - 1) (tail xs)

lastButOne :: [a] -> a
lastButOne xs =
    if length xs <= 2
    then head xs
    else lastButOne (tail xs)
