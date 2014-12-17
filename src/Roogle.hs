module Roogle where

import Roogle.Types
import Roogle.Scope
import Roogle.Document

import System.Environment
import Control.Monad.IO.Class
import Data.Char
import Text.Regex

--- should have Scope -> Doc instead of [Code] -> Doc
--- desireable: [Code] -> Scope -> Doc, Code -> Scope -> Doc
extractTypeSignature :: String -> String -> Maybe String
extractTypeSignature ptn str = case typeSignatureMatch ptn str of
    Just v -> Just $ toDoc v
    Nothing  -> Nothing

typeSignatureMatch :: String -> Code -> Maybe String
typeSignatureMatch ptn str = case matchRegexAll (mkRegex ptn) str of
                       Just (_, _, v, _) -> Just v
                       Nothing -> Nothing

compactMaybe :: [Maybe a] -> [a]
compactMaybe [] = []
compactMaybe (x:xs) = case x of
                        Just v -> [v] ++ compactMaybe xs
                        Nothing -> compactMaybe xs

extractTypeSignatures :: String -> [Code] -> Doc
extractTypeSignatures ptn strs = compactMaybe (map (\x -> (extractTypeSignature ptn x)) strs)

extractTypeSignatureWithSpecifiedPattern :: String -> (String -> Maybe String)
extractTypeSignatureWithSpecifiedPattern = extractTypeSignature

typeSignaturePattern1 :: String
typeSignaturePattern1 = "typesig "
