main :: IO ()
main = do
       name <- fmap (map toUpper) getLine
       putStrLn (name)
       return ()