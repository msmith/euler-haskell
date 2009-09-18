import System.Environment
import Solutions
import Data.List

main :: IO ()
main = do
    arg <- parseArg
    case arg of
        Just(n)  -> printIt $ filter ((==n) . num) solutions
        Nothing  -> printIt $ solutions

parseArg :: IO (Maybe Integer)
parseArg = do
    args <- getArgs
    return $ case args of
        [arg] -> Just(read arg)
        _     -> Nothing

printIt :: [Solution] -> IO ()
printIt ss = do
    mapM_ (\x -> format x >>= putStrLn) $ ss
        where
            format sol = do
                ar <- solution sol
                return $ n ++ (show ar) ++ (check (ar == (expected sol)))
                    where
                        n = (show $ num sol) ++ ": "
                        check True = ""
                        check False = "  BZZT!"
