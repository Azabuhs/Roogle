import Roogle
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    doc <- readFile $ head args
    putStrLn (unlines $ extractTypeSignatures typeSignaturePattern1 $ lines doc)
