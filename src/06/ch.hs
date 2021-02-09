data Color = Red | Green | Blue
instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"

instance Read Color where
    --readsPrec _ value =
        --tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
            --where tryParse [] = []
                  --tryParse ((attempt, result):xs) =
                      --if take (length attempt) value == attempt
                      --then [(result, drop (length attempt) value)]
                      --else tryParse xs
    readsPrec _ [] = []
    readsPrec _ s | take 3 s == "Red"   = [(Red,   drop 3 s)]
                  | take 5 s == "Green" = [(Green, drop 5 s)]
                  | take 4 s == "Blue"  = [(Blue,  drop 4 s)]
                  | otherwise = []


colorEq :: Color -> Color -> Bool
colorEq Red Red     = True
colorEq Green Green = True
colorEq Blue Blue   = True
colorEq _ _         = False

stringEq :: String -> String -> Bool
stringEq "" ""         = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq _ _           = False

class BasicEq a where
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)

instance BasicEq3 Bool where
    isEqual3 x y = x == y

instance BasicEq3 Color where
    isNotEqual3 Red Red     = False
    isNotEqual3 Green Green = False
    isNotEqual3 Blue Blue   = False
    isNotEqual3 _ _         = True

main = do
    putStrLn "Please enter a Double:"
    inpStr <- getLine
    let inpDouble = (read inpStr)::Double
    putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))

data CannotShow = CannotShow
                deriving (Show)

data CannotDeriveShow = CannotDeriveShow CannotShow

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
               deriving (Show)
