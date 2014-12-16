module Roogle where
import System.Environment
import Control.Monad.IO.Class
import Data.Char
import Text.Regex
import Text.Regex.Posix
import Data.Map (Map)

type Document = [String]
type TypeSignature = String
data Scope = EmptyScope
            | Class String [Scope]
            | Module String [Scope]
            | Method String TypeSignature deriving (Show, Eq)
type Wait = String
-- head contexts is the current Context
-- as encounter Wait, tail contexts
-- Wait is "end" in most cases
type Context = Map Scope Wait
type Contexts = [Context]

extractTypeSignature :: String -> String -> Maybe String
extractTypeSignature ptn str = case typeSignatureMatch ptn str of
    Just v -> Just $ toDoc v
    Nothing  -> Nothing

toDoc :: String -> String
toDoc str = subRegex (mkRegex "=>")
          (subRegex (mkRegex ",")
            (subRegex (mkRegex "\\[")
              (subRegex (mkRegex "\\]")
                (subRegex (mkRegex ":") str
                  " ::") "") "") " ->") "->"

scopeToDoc :: Scope -> String
scopeToDoc (Class klass scopes) = unlines $ map (\x -> (klass ++ scopeToDoc x)) scopes
scopeToDoc (Module mdl scopes) = unlines $ map (\x -> (mdl ++ scopeToDoc x)) scopes
scopeToDoc (Method meth typesig) = "#" ++ meth ++ " :: " ++ typesig

--
-- EmptyScope
-- Class className [EmptyScope]
-- Class className [Method name typesig, Method name typesig ...]
--
toScopeList :: Scope -> [Scope]
toScopeList s = filter (\x -> x /= EmptyScope) $ replicate 1 s

walk :: [String] -> Scope
walk = foldl accum EmptyScope

accum :: Scope -> String -> Scope
accum EmptyScope str = mkScope str
accum (Class x [EmptyScope]) str = Class x $ toScopeList $ accum EmptyScope str
accum (Module x [EmptyScope]) str = Class x $ toScopeList $ accum EmptyScope str
accum (Class x s) str = Class x (s ++ (toScopeList $ accum EmptyScope str))
accum (Module x s) str = Module x (s ++ (toScopeList $ accum EmptyScope str))

encounterClass :: String -> Bool
encounterClass str = str =~ "class" :: Bool
encounterModule :: String -> Bool
encounterModule str = str =~ "module" :: Bool
encounterMethod :: String -> Bool
encounterMethod str = str =~ "typesig" :: Bool

mkScope :: String -> Scope
mkScope str
  | encounterClass str = mkClassScope str
  | encounterModule str = mkModuleScope str
  | encounterMethod str = mkMethodScope str
  -- any line with no class, module and typesig
  | otherwise = EmptyScope

mkClassScope :: String -> Scope
mkClassScope str = Class (getClassName str) [EmptyScope]

mkModuleScope :: String -> Scope
mkModuleScope str = Module (getModuleName str) [EmptyScope]

mkMethodScope :: String -> Scope
mkMethodScope str = Method (getMethodName str) (getMethodTypesignature str)

getClassName :: String -> String
getClassName str = case str =~ "class " :: (String, String, String) of
                     (_, _, klass) -> klass
getModuleName :: String -> String
getModuleName str = case str =~ "module " :: (String, String, String) of
  (_, _, mdl) -> mdl
getMethodName :: String -> String
getMethodName str = case str =~ "(?<==)(.*)(?=:)" :: (String, String, String) of
  (_, name, _) -> name
getMethodTypesignature :: String -> String
getMethodTypesignature str = case str =~ ": " :: (String, String, String) of
  (_, _, sig) -> toDoc sig

typeSignatureMatch :: String -> String -> Maybe String
typeSignatureMatch ptn str = case matchRegexAll (mkRegex ptn) str of
                       Just (_, _, v, _) -> Just v
                       Nothing -> Nothing

compactMaybe :: [Maybe a] -> [a]
compactMaybe [] = []
compactMaybe (x:xs) = case x of
                        Just v -> [v] ++ compactMaybe xs
                        Nothing -> compactMaybe xs

extractTypeSignatures :: String -> [String] -> [String]
extractTypeSignatures ptn strs = compactMaybe (map (\x -> (extractTypeSignature ptn x)) strs)

extractTypeSignatureWithSpecifiedPattern :: String -> (String -> Maybe String)
extractTypeSignatureWithSpecifiedPattern = extractTypeSignature

typeSignaturePattern1 :: String
typeSignaturePattern1 = "typesig "
