module Roogle.Types where

type Doc = [String]
type TypeSignature = String
type Code = String
data Scope = EmptyScope
            | Class String [Scope]
            | Module String [Scope]
            | Method String TypeSignature deriving (Show, Eq)
