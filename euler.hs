import System.Environment
import Solutions
import Data.List

main :: IO ()
main = do
    arg <- parseArg
    printIt $ case arg of
        Just(n)  -> filter ((==n) . num) solutions
        Nothing  -> solutions

parseArg :: IO (Maybe Integer)
parseArg = do
    args <- getArgs
    return $ case args of
        [arg] -> Just(read arg)
        _     -> Nothing

printIt :: [Solution] -> IO ()
printIt ss = mapM_ (\x -> format x >>= putStrLn) ss
        where
            format s = do
                let n = num s
                let e = expected s
                sol <- solution s
                return $ show n ++ ": " ++ show sol ++ check (sol == e)
                    where
                        check True = ""
                        check False = "  BZZT!"
