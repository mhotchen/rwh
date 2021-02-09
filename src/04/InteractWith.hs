import           Data.Bits          (shiftL, (.&.), (.|.))
import           Data.Char          (isUpper, ord, toUpper)
import           System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "error: exactly two arguments needed"

          myFunction input = unlines (splitLines input)

splitLines :: String -> [String]
splitLines [] = []
splitLines cs = let (pre, suf) = break isLineTerminator cs
                in  pre : case suf of
                    ('\r':'\n':rest) -> splitLines rest
                    ('\r':rest)      -> splitLines rest
                    ('\n':rest)      -> splitLines rest
                    _                -> []

isLineTerminator c = c == '\r' || c == '\n'

a `plus` b = a + b

data a `Pair` b = a `Pair` b
                  deriving (Show)

foo = Pair 1 2
bar = True `Pair` "quux"

res1 = 1 `plus` 2
res2 = plus 1 2

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = let (pre, suf) = span p xs
                 in pre : splitWith p (drop 1 suf)

isNumber :: Char -> Bool
isNumber '1' = True
isNumber '2' = True
isNumber '3' = True
isNumber '4' = True
isNumber '5' = True
isNumber '6' = True
isNumber '7' = True
isNumber '8' = True
isNumber '9' = True
isNumber '0' = True
isNumber _   = False

upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
upperCase []     = []

upperCase2 :: String -> String
upperCase2 xs = map toUpper xs

oddList :: [Int] -> [Int]
oddList (x:xs) | odd x     = x : oddList xs
               | otherwise = oddList xs
oddList _                  = []

mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc


foldSum xs = foldl (+) 0 xs

base = 65521

adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b _      = (b `shiftL` 16) .|. a


foldAdler32 xs = let (a, b) = foldl step (1, 0) xs
                 in (b `shiftL` 16) .|. a
    where step (a, b) x = let a' = a + (ord x .&. 0xff)
                          in (a' `mod` base, (a' + b) `mod` base)


data Expr = I Int
        | B Bool           -- boolean constants
        | Add Expr Expr
        | Mul Expr Expr
        | Eq  Expr Expr    -- equality test

eval :: Expr -> Maybe (Either Int Bool)
eval (I i) = Just (Left i)
eval (B b) = Just (Right b)
eval (Add e1 e2) = case (e1, e2) of
                       (B _, _)   -> Nothing
                       (_, B _)   -> Nothing
                       (i, j)     -> case (eval i, eval j) of
                                        (Just (Left i), Just (Left j)) -> Just (Left (i + j))
                                        _                              -> Nothing
eval (Mul e1 e2) = case (e1, e2) of
                       (B _, _)   -> Nothing
                       (_, B _)   -> Nothing
                       (i, j)     -> case (eval i, eval j) of
                                        (Just (Left i), Just (Left j)) -> Just (Left (i * j))
                                        _                              -> Nothing
eval (Eq e1 e2)  = case (e1, e2) of
                       (I _, _)   -> Nothing
                       (_, I _)   -> Nothing
                       (i, j)     -> case (eval i, eval j) of
                                        (Just (Right i), Just (Right j)) -> Just (Right (i == j))
                                        _                                -> Nothing

--Just (Left (eval e1 + eval e2))
--eval (Mul e1 e2) = eval e1 * eval e2
--

suffixes :: [a]        -> [[a]]
suffixes    xs@(_:xs') = xs : suffixes xs'
suffixes    _          = []








