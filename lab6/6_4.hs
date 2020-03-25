main :: IO ()
main = do
       putStrLn "What is your name?"
       name <- getLine
       putStrLn ("Hello, " ++ name ++ "!")
       if name == "" then
        return ()
       else
        main
     