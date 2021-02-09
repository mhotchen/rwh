import           Data.Char (toUpper)
import           System.IO

main :: IO ()
main = do
    inHandle <- openFile "in.txt" ReadMode
    outHandle <- openFile "output.txt" WriteMode
    mainloop inHandle outHandle
    hClose inHandle
    hClose outHandle

mainloop :: Handle -> Handle -> IO ()
mainloop inHandle outHandle =
    do inIsEof <- hIsEOF inHandle
       if inIsEof
       then return ()
       else do inLine <- hGetLine inHandle
               hPutStrLn outHandle (map toUpper inLine)
               mainloop inHandle outHandle
