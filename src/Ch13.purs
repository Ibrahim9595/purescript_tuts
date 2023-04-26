module Ch13 where 

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Show, Unit, discard, show, ($), (/))

class Functor f where
  map :: ∀ a b. (a -> b) -> (f a -> f b)

infixl 4 map as <$> 
  
instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x


data Either a b = Left a | Right b 

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance functorEither :: Functor (Either a) where
  map _ (Left x) = Left x
  map f (Right y) = Right $ f y


data Tuple a b = Tuple a b

derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple x y) = Tuple x (f y)

data Threeple a b c = Threeple a b c

derive instance genericThreeple :: Generic (Threeple a b c) _
instance showThreeple :: (Show a, Show b, Show c) => Show (Threeple a b c) where
  show = genericShow

instance functorThreeple :: Functor (Threeple a b) where
  map f (Threeple x y z) = Threeple x y $ f z

test :: Effect Unit 
test = do
  log $ show $ (_/2) <$> Threeple 10 20 40
