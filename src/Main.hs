import Roogle
import System.Environment(getArgs)

rstrip :: String -> String
rstrip = reverse . dropWhile (=='\n') . reverse

main :: IO ()
main = do
    args <- getArgs
    doc <- readFile $ head args
    putStrLn (rstrip $ unlines $ extractTypeSignatures typeSignaturePattern1 $ lines doc)
