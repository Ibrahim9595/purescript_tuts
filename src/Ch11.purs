module Ch11 where

import Ch5 (singleton)
import Ch7a (Maybe(..))
import Data.Foldable (class Foldable)
import Data.List (List(..), foldMap, foldl, foldr, (:))
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty (NonEmpty, (:|))
import Effect (Effect)
import Effect.Class.Console (log)
import Prelude (class Ord, class Semiring, type (~>), Unit, discard, negate, otherwise, show, zero, ($), (+), (-), (<<<), (<>), (>))

reverse :: List ~> List
reverse  = foldl (\ rl x -> x : rl) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y | x > y = x 
        | otherwise = y

-- findMax :: ∀ a. Ord a => List a -> Maybe a
-- findMax Nil = Nothing
-- findMax l@(first:_) = Just $ go first l where
--   go mx Nil = mx
--   go mx (x:xs) = go (max x mx) xs

findMax :: ∀ a. Ord a => List a -> Maybe a
findMax Nil = Nothing
findMax l@(x:_) = Just $ foldl max x l

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) ->  NonEmpty f a -> a
foldl1 f ( x :| xs) = foldl f x xs 
-- x:|xs === NonEmpty x xs

-- findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
-- findMaxNE (NonEmptyList (NonEmpty first l)) = foldl max first l

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList ne) = foldl1 max ne


-- sum :: List Int -> Int
-- sum = go 0 where
--   go acc Nil = acc
--   go acc (x:xs) = go (acc + x) xs

sum :: ∀ a f. Semiring a => Foldable f => f a -> a
sum  = foldl (+) zero


data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf x) = singleton x
toList (Node left right) = toList left <> toList right 

instance foldableTree :: Foldable Tree where
  foldl f acc = foldl f acc <<< toList
  foldr f acc = foldr f acc <<< toList 
  foldMap f   = foldMap f   <<< toList 

test :: Effect Unit
test = do
    log $ show $ sum (Node (Node (Leaf 5) (Node (Leaf (-1)) (Leaf 14))) (Leaf 99))