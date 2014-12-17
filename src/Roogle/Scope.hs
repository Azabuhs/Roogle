module Roogle.Scope where
import Roogle.Types
import Roogle.Document
import Text.Regex
import Text.Regex.Posix

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
