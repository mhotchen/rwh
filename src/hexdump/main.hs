import           Data.Char (chr, ord)
import           Numeric   (showHex)

charsPerLine = 40

newtype HexChar = HexChar (Char, String)

charAsHexString :: Char -> String
charAsHexString c = showHex (ord c) ""



newtype Character = Character Int
newtype Hexidecimal = Hexidecimal Int

class CharacterConversion a where
    toCharacter :: a -> Character
    fromCharacter :: Character -> a

instance CharacterConversion Int where
    toCharacter = character
    fromCharacter (Character i) = i

instance CharacterConversion Char where
    toCharacter c = character (ord c)
    fromCharacter (Character i) = chr i

instance CharacterConversion Hexidecimal where
    toCharacter (Hexidecimal i) = character i
    fromCharacter (Character i) = hexidecimal i

character :: Int -> Character
character = Character

hexidecimal :: Int -> Hexidecimal
hexidecimal = Hexidecimal


hexdump :: Int -> String -> String

--hexdump charsPerLine s = let hexString = map charAsHexString s
                             --zipped :: [HexChar]
                             --zipped = map (HexChar) (zip s hexString)
                         --in foldr (doSomething . (toLines charsPerLine)) [[]] zipped
    --where doSomething :: [[HexChar]] -> String -> String
          --doSomething (c, hex) s = s ++ [c] ++ " . " ++ hex
          --toLines :: Int -> HexChar -> [[HexChar]] -> [[HexChar]]
          --toLines lineLength c (cp:cs) = if lineLength == charsPerLine
                                         --then [c] : cp : cs
                                         --else (c : cp) : cs


str = "Classic I/O in Haskell \
\Let’s get started with I/O in Haskell by looking at a program that appears to be sur- \
\prisingly similar to I/O in other languages such as C or Perl: \
\-- file: ch07/basicio.hs \
\-- main = do \
\-- putStrLn \"Greetings! What is your name?\" \
\-- inpStr <- getLine \
\-- putStrLn $ \"Welcome to Haskell, \" ++ inpStr ++ \"!\" \
\-- You can compile this program to a standalone executable, run it with runghc , or invoke \
\-- main from within ghci . Here’s a sample session using runghc :"
