type BookID = Int
type MagazineID = Int
type CustomerID = Int
type Title = String
type Authors = [String]
type ReviewBody = String

data Book = Book BookID Title Authors
            deriving (Show)

data Magazine = Magazine MagazineID Title Authors
                deriving (Show)

data BookReview = BookReview Book CustomerID ReviewBody
                  deriving (Show)

data MagazineReview = MagazineReview Magazine CustomerID ReviewBody
                      deriving (Show)

--data Review = BookReview Book CustomerID ReviewBody
--            | MagazineReview Magazine CustomerID ReviewBody

data Record = BookRecord Book BookReview
            | MagazineRecord Magazine MagazineReview

book     = Book 123445 "Matt writes a book" ["Matt Hotchen"]
magazine = Magazine 89028903 "Grenades weekly" ["Duke Nukem", "Mr Grenadey Head"]

bookReview     = BookReview book 123 "not great"
magazineReview = MagazineReview magazine 123 "explosive stuff!"

bookRecord     = BookRecord book bookReview
magazineRecord = MagazineRecord magazine magazineReview

getReview :: Record -> BookReview -- | MagazineReview
getReview (BookRecord book review) = review
--getReview (MagazineRecord magazine review) = review

isFightClub :: Book -> Bool
isFightClub (Book _ "Fight Club" _) = True
isFightClub _                       = False

bookId          (Book id _ _)         = id
bookTitle       (Book _ title _)      = title
bookFirstAuthor (Book _ _ (author:_)) = author

-- data Bool = True | False

type CardHolder = String
type CardNumber = String
type Address = [String]
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving(Show)

data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)

myNot True  = False
myNot False = True

sumList (x:xs) = x + sumList xs
sumList []     = 0

data Customer = Customer {
    customerID      :: CustomerID,
    customerName    :: String,
    customerAddress :: Address
} deriving (Show)

customer1 = Customer 123 "Matt" ["abc","def"]
customer2 = Customer {
          customerID = 123,
          customerName = "Matt",
          customerAddress = ["abxc","def"]
}

--data Maybe a = Just a
--             | Nothing

data List a = Cons a (List a)
            | Nil deriving (Show)

toList :: List a -> [a]
toList (Cons a list) = a:toList list
toList Nil           = []

listFromCons = toList (Cons 2 (Cons 1 (Cons 0 Nil)))

data ATree a = ANode a (ATree a) (ATree a)
             | Empty
               deriving (Show)

simpleTree = ANode "parent" (ANode "left child" Empty Empty) (ANode "right child" Empty Empty)

data MaybeTree a = MaybeTree a (Maybe (MaybeTree a)) (Maybe (MaybeTree a))
                   deriving (Show)

maybeTree = MaybeTree "parent" (Just (MaybeTree "left child" Nothing Nothing)) (Just (MaybeTree "right child" Nothing Nothing))
deepMaybeTree = MaybeTree "parent"
                          (Just (MaybeTree "left child"
                                           (Just (MaybeTree "left child"
                                                            (Just (MaybeTree "left child" Nothing Nothing))
                                                            Nothing))
                                           Nothing))
                          (Just (MaybeTree "right child"
                                           Nothing
                                           (Just (MaybeTree "right child" Nothing Nothing))))

maxDepth :: MaybeTree a -> Int
maxDepth (MaybeTree _ Nothing  Nothing)  = 0
maxDepth (MaybeTree _ (Just l) Nothing)  = 1 + maxDepth l
maxDepth (MaybeTree _ Nothing  (Just r)) = 1 + maxDepth r
maxDepth (MaybeTree _ (Just l) (Just r)) = 1 + if maxDepth l > maxDepth r
                                               then maxDepth l
                                               else maxDepth r

mySecond :: [a] -> a
mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)

safeSecond :: [a] -> Maybe a
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance

lend2 amount balance = if amount < reserve * 0.5
                       then Nothing
                       else Just newBalance
    where reserve    = 100
          newBalance = balance - amount

lend3 amount balance
     | amount <= 0            = Nothing
     | amount < reserve * 0.5 = Nothing
     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount

pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ word ++ "s"

fromMaybe defaultVal wrapped =
    case wrapped of
        Nothing    -> defaultVal
        Just value -> value

nodesAreSame (ANode a _ _) (ANode b _ _)
    | a == b     = Just a
nodesAreSame _ _ = Nothing

lendsAreSame lend1 lend2 lend3 = lend1 == lend2 && lend2 == lend3


length2 :: [a] -> Int
length2 []     = 0
length2 (_:xs) = 1 + length2 xs

mean :: [Double] -> Double
mean []  = 0
mean [x] = x
mean xs  = sum xs / fromIntegral (length2 xs)

palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

intersperse :: a -> [[a]] -> [a]
intersperse _ []     = []
intersperse _ [x]    = x
intersperse c (x:xs) = x ++ c:intersperse c xs


data Direction = DLeft
               | DRight
               | DStraight

data Point = Point Int Int
newtype Points = Points [(Point, Point, Point)]

-- I am not cut out for this...
--
-- a = 5,3
-- b = 2,7
-- c = 5,5
-- DRight
--
--10 . . . . . . . . . .
-- 9 . . . . . . . . . .
-- 8 . . . . . . . . . .
-- 7 . b . . . . . . . .
-- 6 . . . . . . . . . .
-- 5 . . . . c . . . . .
-- 4 . . . . . . . . . .
-- 3 . . . . a . . . . .
-- 2 . . . . . . . . . .
-- 1 . . . . . . . . . .
--   1 2 3 4 5 6 7 8 9 10
--
-- a = 7,8
-- b = 3,2
-- c = 5,2
-- DLeft
--
--10 . . . . . . . . . .
-- 9 . . . . . . . . . .
-- 8 . . . . . . a . . .
-- 7 . . . . . . . . . .
-- 6 . . . . . . . . . .
-- 5 . . . . . . . . . .
-- 4 . . . . . . . . . .
-- 3 . . . . . . . . . .
-- 2 . . b . c . . . . .
-- 1 . . . . . . . . . .
--   1 2 3 4 5 6 7 8 9 10
--
-- a = 1,8
-- b = 6,8
-- c = 8,1
-- DRight
--
--10 . . . . . . . . . .
-- 9 . . . . . . . . . .
-- 8 a . . . . b . . . .
-- 7 . . . . . . . . . .
-- 6 . . . . . . . . . .
-- 5 . . . . . . . . . .
-- 4 . . . . . . . . . .
-- 3 . . . . . . . . . .
-- 2 . . . . . . . . . .
-- 1 . . . . . . . c . .
--   1 2 3 4 5 6 7 8 9 10

direction :: Point -> Point -> Point -> Maybe Direction
direction (Point ax ay) (Point bx by) (Point cx cy) = Just DLeft
