main :: IO ()
main = (putStrLn "Hello, World!") >> ((putStrLn "All good.") >> (putStrLn "Nice!"))
