module Tokens
    (
      Token(..)
    )
where
data Token =  Operator Char |
              Number Double |
              OpenBrackets  |
              CloseBrackets
              deriving (Show, Eq)
