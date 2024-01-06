module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Prelude (type (~>), max, negate, otherwise, (+), (-), (<), (<<<), (==), (>), (>>>))

flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: ∀ a b. a -> b -> a
const x _ = x

apply :: ∀ a b. (a -> b) -> a -> b
apply f = f

infixr 0 apply as $

applyFlipped :: ∀ a b. a -> (a -> b) -> b
applyFlipped a f = f a

infixl 1 applyFlipped as #

singleton :: ∀ a. a -> List a
singleton x = x : Nil

null :: ∀ a. List a -> Boolean
null Nil = true

null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc Nil y = singleton y

snoc (x : xs) y = x : snoc xs y

length :: ∀ a. List a -> Int
length Nil = 0

length (_ : xs) = 1 + length xs

length' :: ∀ a. List a -> Int
length' l = g 0 l
  where
  g n Nil = n

  g n (_ : xs) = g (n + 1) xs

head :: ∀ a. List a -> Maybe a
head Nil = Nothing

head (x : _) = Just x

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing

tail (_ : xs) = Just xs

last :: ∀ a. List a -> Maybe a
last Nil = Nothing

last (x : Nil) = Just x

last (_ : xs) = last xs

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing

init l = Just $ go l
  where
  go Nil = Nil

  go (_ : Nil) = Nil

  go (x : xs) = x : go xs

uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing

uncons (x : xs) = Just { head: x, tail: xs }

index :: ∀ a. List a -> Int -> Maybe a
index Nil _ = Nothing

index (x : _) 0 = Just x

index (_ : xs) i = if i < 0 then Nothing else index xs (i - 1)

infixl 8 index as !!

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing

findIndex pred l = go 0 l
  where
  go _ Nil = Nothing

  go i (x : xs) = if (pred x) then Just i else (go (i + 1) xs)

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing

findLastIndex pred l = go 0 Nothing l
  where
  go _ target Nil = target

  go i target (x : xs) = go (i + 1) (if pred x then (Just i) else target) xs

reverse :: List ~> List
reverse Nil = Nil

reverse l = go Nil l
  where
  go n Nil = n

  go n (x : xs) = go (x : n) xs

concat :: ∀ a. List (List a) -> List a
concat Nil = Nil

concat (Nil : xss) = concat xss

concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil

filter pred (x : xs)
  | pred x = x : filter pred xs
  | otherwise = filter pred xs

catMaybes :: ∀ a. List (Maybe a) -> List a
catMaybes Nil = Nil

catMaybes (x : xs) = case x of
  Nothing -> catMaybes xs
  Just y -> y : catMaybes xs

range :: Int -> Int -> List Int
range s e
  | s == e = singleton s
  | s > e = s : range (s + (-1)) e
  | s < e = s : range (s + 1) e
  | otherwise = Nil

take :: ∀ a. Int -> List a -> List a
take n = reverse <<< go Nil (max 0 n)
  where
  go acc _ Nil = acc

  go acc 0 _ = acc

  go acc n' (x : xs) = go (x : acc) (n' - 1) xs

drop :: ∀ a. Int -> List a -> List a
drop _ Nil = Nil

drop 0 l = l

drop n (_ : xs) = drop (n - 1) xs

takeWhile :: ∀ a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil

takeWhile pred (x : xs) = if pred x then x : takeWhile pred xs else Nil

dropWhile :: ∀ a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil

dropWhile pred l@(x : xs) = if pred x then dropWhile pred xs else l

takeEnd :: ∀ a. Int -> List a -> List a
takeEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil

  go (x : xs) =
    go xs
      # \(Tuple c nl) -> Tuple (c + 1) (if c < n then x : nl else nl)

dropEnd :: ∀ a. Int -> List a -> List a
dropEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil

  go (x : xs) =
    go xs
      # \(Tuple c nl) -> Tuple (c + 1) (if c < n then nl else (x : nl))

zip :: ∀ a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil

zip _ Nil = Nil

zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: ∀ a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip = go
  where
  go Nil = Tuple Nil Nil

  go (Tuple x y : ts) =
    go ts
      # \(Tuple xs ys) -> Tuple (x : xs) (y : ys)
