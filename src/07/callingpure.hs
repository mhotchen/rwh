name2reply :: String -> String
name2reply name =
    "pleased to meet you, " ++ name ++ ".\n" ++
    "your name contains " ++ charcount ++ " characters"
    where charcount = show (length name)

main = do
    putStrLn "Greetings, what's your name?"
    input <- getLine
    let outStr = name2reply input
    putStrLn outStr
