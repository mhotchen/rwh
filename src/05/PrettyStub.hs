{-# LANGUAGE FlexibleInstances #-}

module PrettyStub
    (
      Doc
    , string
    , text
    , double
    , list
    , object
    , renderJValue
    , compact
    , compress
    , fill
    , result
    , JSON
    , JAry(fromJAry)
    , jary
    , What(..)
    ) where

import           Data.Bits (shiftR, (.&.))
import           Data.Char (ord)
import           JJson
import           Numeric   (showHex)

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show, Eq)

empty :: Doc
empty = Empty

string :: String -> Doc
string = enclose '"' '"' . foldr ((<->) . oneChar) empty

text :: String -> Doc
text = Text

char :: Char -> Doc
char = Char

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

list :: [a] -> Doc
list as = undefined

object :: (String, a) -> Doc
object o = undefined

(<->) :: Doc -> Doc -> Doc
Empty <-> y = y
x <-> Empty = x
x <-> y = x `Concat` y

enclose :: Char -> Char -> Doc -> Doc
enclose l r s = char l <-> s <-> char r

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> char c
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\', b])

compress :: Doc -> Doc
compress (a `Concat` Empty) = compress a
compress (Empty `Concat` b) = compress b
compress (a `Concat` b)     = let ac = compress a
                                  bc = compress b
                              in case (ac, bc) of
                                  (Char a, Char b) -> Text (a:b:"")
                                  (Char a, Text b) -> Text (a:b)
                                  (Text a, Char b) -> Text (a ++ [b])
                                  (Text a, Text b) -> Text (a ++ b)
                                  _                -> ac `Concat` bc
compress other              = other

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
    where d = ord c

smallHex :: Int -> Doc
smallHex x =    text "\\u"
            <-> text (replicate (4 - length h) '0')
            <-> text h
    where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) <-> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

fsep :: [Doc] -> Doc
fsep = foldr f empty
    where f :: Doc -> Doc -> Doc
          f x y = x <-> group line <-> y

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat ` y) = flatten x `Concat` flatten y
flatten Line            = char ' '
flatten (x `Union` _)   = flatten x
flatten other           = other

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                  Empty        -> transform ds
                  Char c       -> c : transform ds
                  Text s       -> s ++ transform ds
                  Line         -> '\n' : transform ds
                  a `Concat` b -> transform (a:b:ds)
                  _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) =
              case d of
                  Empty         -> best col ds
                  Char c        -> c : best (col + 1) ds
                  Text s        -> s ++ best (col + length s) ds
                  Line          -> '\n' : best 0 ds
                  a `Concat ` b -> best col (a:b:ds)
                  a `Union` b   -> nicest col (best col (a:ds))
                                              (best col (b:ds))
          best _ _ = ""
          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                         where least = min width col


fill :: Int -> Char -> Doc -> Doc
fill _ _ Empty = Empty
fill lineLength fillChar (Char c) = Text (c : fillString (lineLength - 1) fillChar)
fill lineLength fillChar (Text s) = if length s < lineLength
                                    then Text (s ++ fillString (lineLength - length s) fillChar)
                                    else Text s

fillString :: Int -> Char -> String
fillString 0 _ = ""
fillString l c = c : fillString (l - 1) c


--fill :: Int -> Char -> Doc -> Doc
--fill _ _ Empty = Empty
--fill _ _ Char c = Empty
--fill width char Text s = Empty
        --filler col (d:ds) =
              --case d of
                  --Empty         -> filler col ds
                  --Char c        -> Char c : filler (c : col) ds
                  --Text s        -> Text s : filler (s ++ col) ds
                  --a `Concat ` b -> filler col a `Concat` filler col b : filler col ds
                  --a `Union` b   -> filler col a `Union` filler col b : filler col ds
                  --Line          -> addFill col width c : filler "" ds
          --filler _ [] = Empty
          --addFill :: Int -> Int -> Char -> Doc
          --addFill length width c = let l = length col
                                   --in if l col < width
                                   --then Concat (Text (fillString (width - l) c)) Line
                                   --else Line
              --where fillString :: Int -> Char -> String
                    --fillString 0 _ = ""
                    --fillString l c = c:fillString (l - 1) c

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n':_) = True
w `fits` (c:cs) = (w - 1) `fits` cs

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <-> p) : punctuate p ds

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series l r item = enclose l r . fsep . punctuate (char ',') . map item

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray as)   = series '[' ']' renderJValue as
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name, val) = string name
                            <-> text ":"
                            <-> renderJValue val



result :: JValue
result = JObject [
        ("query", JString "giraffe pictures"),
        ("estimatedCount", JNumber 987234),
        ("moreResults", JBool True),
        ("results", JArray [
            JObject [
                ("title", JString "giraffe"),
                ("snippet", JString "giraffes are cool"),
                ("url", JString "https://mhn.me")
            ]
        ])
    ]
