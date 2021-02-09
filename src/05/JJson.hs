module JJson
    (
      JValue(..)
    , obj
    , findAllStrings
    , jvalueToString
    ) where

import           Control.Arrow (second)
import           Data.List     (intercalate)

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
              deriving (Eq, Ord, Show)

str = JString "foo\r\n{\"json\":true}"
num = JNumber 2.7
bool = JBool True
arr = JArray (jary [JNumber 2.7, JString "hello", JNull, JObject (jobj [("hello", JString "world")])])
obj = JObject
    ( jobj [
      ("str",  str)
    , ("num",  num)
    , ("bool", bool)
    , ("arr",  arr)
    , ("null", JNull)
    ])

findAllStrings :: JValue -> [String]
findAllStrings x = r x []
    where r :: JValue -> [String] -> [String]
          r (JString x)  xs = x:xs
          r (JArray as)  xs = concatMap . findAllStrings . fromJAry as ++ xs
          r (JObject os) xs = concatMap . findAllStrings . snd . fromJObj os ++ xs
          r _            xs = xs

jvalueToString :: JValue -> String
jvalueToString JNull         = "null"
jvalueToString (JBool True)  = "true"
jvalueToString (JBool False) = "false"
jvalueToString (JNumber n)   = show n
jvalueToString (JString x)   = "\"" ++ escape x ++ "\""
jvalueToString (JArray as)   = "[" ++ intercalate "," . map (jvalueToString . fromJAry) as ++ "]"
jvalueToString (JObject os)  = "{" ++ intercalate "," . map (objectValueToString . fromJObj) os ++ "}"
    where objectValueToString :: (String, JValue) -> String
          objectValueToString (key, value) = "\"" ++ escape key ++ "\":" ++ jvalueToString value

escape :: String -> String
escape []       = []
escape ('\\':s) = "\\\\" ++ escape s
escape ('\"':s) = "\\\"" ++ escape s
escape ('\r':s) = "\\r"  ++ escape s
escape ('\n':s) = "\\n"  ++ escape s
escape (c:s)    = c:escape s

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

data JSONError = JSONError String
               deriving (Show)

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b

instance JSON JValue where
    toJValue = id
    fromJValue = Right


instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id

--instance JSON String where
    --toJValue _ = JString "a"
    --fromJValue (JString s) = Right "a"

instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined

--instance (JSON a) => JSON [(String, a)] where
    --toJValue = undefined
    --fromJValue = undefined

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _           = Left (JSONError "not a JSON number")


newtype JAry a = JAry { fromJAry :: [a] }
               deriving (Eq, Ord, Show)
jary :: [a] -> JAry a
jary = JAry

newtype JObj a = JObj { fromJObj :: [(String, a)] }
               deriving (Eq, Ord, Show)
jobj = JObj

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _                 = Left (JSONError "Not a JSON array")

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a)  = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case f x of
                      Left err -> Left err
                      Right y -> case mapEithers f xs of
                                     Left err -> Left err
                                     Right ys -> Right (y:ys)
mapEithers _ _ = Right []


instance (JSON a) => JSON (JAry a) where
    toJValue = jaryToJValue
    fromJValue = jaryFromJValue

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj
    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
        where unwrap (k, v) = whenRight ((,) k) (fromJValue v)
    fromJValue _ = Left (JSONError "not a JSON object")
