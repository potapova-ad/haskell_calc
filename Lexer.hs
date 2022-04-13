module Lexer
    (
      lexicallyAnalyse
    )
where
import Data.Char (isDigit, isSpace)
import Data.Maybe
import Tokens (Token, Token(..))

data LexerState = Start          |
                  Digits         |
                  DigitsAfterDot |
                  Operators      |
                  Brackets       |
                  Error
                  deriving (Show)

isOperator :: Char -> Bool
isOperator x = x `elem` ['+','-','*','/']

isBrackets :: Char -> Bool
isBrackets x = x `elem` ['(',')']

lexicallyAnalyse :: String -> Maybe [Token]
lexicallyAnalyse input = consume input Start [] []

consume :: String -> LexerState -> [(LexerState, Char)] -> [Token] -> Maybe [Token]
consume (x:xs) Start history tokens
  | isDigit x    = consume xs Digits ((Start, x) : history) tokens
  | isOperator x = consume xs Operators ((Start, x) : history) tokens
  | isBrackets x = consume xs Brackets ((Brackets, x) : history) tokens
  | isSpace x    = consume xs Start history tokens
  | otherwise    = Nothing

consume [] Digits history tokens = consume [] Start [] (tokens ++ [Number number])
  where number = read (historyAsStr history) :: Double

consume input@(x:xs) Digits history tokens
  | isDigit x = consume xs Digits ((Digits, x) : history) tokens
  | isSpace x = consume xs Digits history tokens
  | x == '.'  = consume xs DigitsAfterDot ((Digits, x) : history) tokens
  | otherwise = consume input Start [] (tokens ++ [Number number])
  where number = read (historyAsStr history) :: Double

consume [] DigitsAfterDot history tokens = consume [] Start [] (tokens ++ [Number number])
  where number = read (historyAsStr history) :: Double

consume input@(x:xs) DigitsAfterDot history tokens
  | isDigit x       = consume xs DigitsAfterDot ((DigitsAfterDot, x) : history) tokens
  | isSpace x       = consume xs DigitsAfterDot history tokens
  | prevChar == '.' = Nothing
  | otherwise       = consume input Start [] (tokens ++ [Number number])
  where (_, prevChar) = head history
        number        = read (historyAsStr history) :: Double

consume [] Operators history tokens = consume [] Start [] (tokens ++ [Operator operator])
  where operator = (head . historyAsStr) history

consume input@(x:xs) Operators history tokens
  | isSpace x = consume xs Operators history tokens
  | otherwise = consume input Start [] (tokens ++ [Operator operator])
  where operator = (head . historyAsStr) history

consume [] Brackets history tokens = consume [] Start [] (tokens ++ [bracketsToken])
  where brackets      = (head . historyAsStr) history
        bracketsToken = case brackets of
            '(' -> OpenBrackets
            ')' -> CloseBrackets

consume input Brackets history tokens = consume input Start [] (tokens ++ [bracketsToken])
  where brackets      = (head . historyAsStr) history
        bracketsToken = case brackets of
            '(' -> OpenBrackets
            ')' -> CloseBrackets

consume [] state history tokens = Just tokens
consume _ _ _ _                 = Nothing

historyAsStr :: [(LexerState, Char)] -> String
historyAsStr history = reverse [ x | (state, x) <- history ]
