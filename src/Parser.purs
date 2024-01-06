module Parser where

import Prelude
import Control.Alt (class Alt, (<|>))
import Data.CodePoint.Unicode (isAlpha, isDecDigit)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray, uncons)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, none, replicate)
import Effect (Effect)
import Effect.Console (log)

type ParserState a
  = Tuple String a

class ParserError (e :: Type) where
  eof :: e
  invalidChar :: String -> e

data PError
  = EOF
  | InvalidChar String

instance parseErrorPError :: ParserError PError where
  eof = EOF
  invalidChar s = InvalidChar s

derive instance geniricPError :: Generic PError _

instance showPError :: Show PError where
  show = genericShow

type ParseFunction e a
  = ParserError e => String -> Either e (ParserState a)

newtype Parser e a
  = Parser (ParseFunction e a)

instance functorParser :: Functor (Parser e) where
  map f p = Parser \s -> (f <$> _) <$> parse p s

instance bindParser :: Bind (Parser e) where
  bind p f =
    Parser \s -> do
      Tuple s1 x <- parse p s
      parse (f x) s1

instance monadParser :: Monad (Parser e)

-- instance applyParser :: Apply (Parser e) where
--   apply :: ∀ a b. Parser e (a -> b) -> Parser e a -> Parser e b
--   apply p1 p2 = Parser \s -> case parse p1 s of
--     Left err -> Left err
--     Right (Tuple s1 h) -> case parse p2 s1 of
--       Left err -> Left err
--       Right (Tuple s2 x) -> Right (Tuple s2 (h x))
instance applyParser :: Apply (Parser e) where
  -- apply p1 p2 = Parser \s -> do
  --   Tuple s1 f <- parse p1 s
  --   Tuple s2 x <- parse p2 s1
  --   pure $ Tuple s2 $ f x
  apply = ap

instance applicativeParser :: Applicative (Parser e) where
  pure x = Parser \s -> pure $ Tuple s x

instance altParser :: Alt (Parser e) where
  alt p1 p2 =
    Parser \s -> case parse p1 s of
      Right x -> Right x
      Left _ -> parse p2 s

char :: ∀ e. Parser e Char
char =
  Parser \s -> case uncons s of
    Nothing -> Left eof
    Just { head, tail } -> Right $ Tuple tail head

twoCharsA :: ∀ e. Parser e (Tuple Char Char)
twoCharsA = Tuple <$> char <*> char

twoCharsB :: ∀ e. Parser e (Tuple Char Char)
twoCharsB = char >>= (\(c1) -> char >>= \(c2) -> pure $ Tuple c1 c2)

twoChars :: ∀ e. Parser e (Tuple Char Char)
twoChars = do
  c1 <- char
  c2 <- char
  pure $ Tuple c1 c2

threeCharsA :: ∀ e. Parser e String
threeCharsA = (\c1 c2 c3 -> fromCharArray [ c1, c2, c3 ]) <$> char <*> char <*> char

threeChars :: ∀ e. Parser e String
threeChars = do
  c1 <- char
  c2 <- char
  c3 <- char
  pure $ fromCharArray [ c1, c2, c3 ]

fail :: ∀ e a. ParserError e => e -> Parser e a
fail e = Parser $ const $ Left e

satisfy :: ∀ e. ParserError e => String -> (Char -> Boolean) -> Parser e Char
satisfy expected pred = char >>= \c -> if pred c then pure c else fail $ invalidChar expected

-- do
--  c <- char
--  if pred c then pure c else Parser \_ -> Left $ invalidChar expected
digit :: ∀ e. ParserError e => Parser e Char
digit = satisfy "digit" (isDecDigit <<< codePointFromChar)

letter :: ∀ e. ParserError e => Parser e Char
letter = satisfy "letter" (isAlpha <<< codePointFromChar)

alphaNum :: ∀ e. ParserError e => Parser e Char
alphaNum = letter <|> digit <|> fail (invalidChar "alphaNum")

count :: ∀ e a f. Traversable f => Unfoldable f => Int -> Parser e a -> Parser e (f a)
count c p
  | c <= 0 = pure none
  | otherwise = sequence (replicate c p)

count' :: ∀ e. Int -> Parser e Char -> Parser e String
count' c p = fromCharArray <$> count c p

parse :: ∀ e a. Parser e a -> ParseFunction e a
parse (Parser f) = f

parse' :: ∀ a. Parser PError a -> ParseFunction PError a
parse' = parse

test :: Effect Unit
test = do
  -- log $ show $ parse' char "ABC" 
  -- log $ show $ parse' twoChars "ABC" 
  -- log $ show $ parse' threeChars "ABC" 
  -- log $ show $ parse' threeChars "A"
  -- log $ show $ parse' (fromCharArray <$> (count 3 char)) "xyz"
  log $ show $ parse' (count' 3 digit) "123456"
  log $ show $ parse' (count' 3 digit) "abc456"
  log $ show $ parse' (count' 4 letter) "Freddy"
  log $ show $ parse' (count' 10 alphaNum) "a1b2c3d4e5"
  log $ show $ parse' (count' 10 alphaNum) "######"
