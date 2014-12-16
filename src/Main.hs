import Roogle

main :: IO ()
main = do
    doc <- readFile "typesig.rb"
    putStrLn (unlines $ extractTypeSignatures typeSignaturePattern1 $ lines doc)
