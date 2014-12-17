module Roogle.Document where
import Roogle.Types
import Text.Regex

toDoc :: Code -> TypeSignature
toDoc str = subRegex (mkRegex "=>")
          (subRegex (mkRegex ",")
            (subRegex (mkRegex "\\[")
              (subRegex (mkRegex "\\]")
                (subRegex (mkRegex ":") str
                  " ::") "") "") " ->") "->"

