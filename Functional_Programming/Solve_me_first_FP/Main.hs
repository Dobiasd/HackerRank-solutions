solveMeFirst :: Integer -> Integer -> Integer
solveMeFirst = (+)

main :: IO ()
main = do
    a <- readLn
    b <- readLn
    let result = solveMeFirst a b
    print result