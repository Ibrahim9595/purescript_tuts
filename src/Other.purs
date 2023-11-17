module Other where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)


errIfMissing :: Maybe String -> String -> Either String String
errIfMissing Nothing err = Left err
errIfMissing (Just s) _ = Right s

fullName :: String -> String -> String -> String
fullName first mid last = first <> " " <> mid <> " " <> last

fullNameEither ::  Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither first mid last = 
    fullName <$> errIfMissing first "First Name is missing"
             <*> errIfMissing mid "Middle Name is missing"
             <*> errIfMissing last "Last Name is missing"

fullNameResp :: Either String String -> String
fullNameResp (Right s) = s
fullNameResp (Left s) = s


allCombinations :: ∀ f a. Applicative f => (a  -> a -> a) -> f a -> f a -> f a
allCombinations f a b = f <$> a <*> b

test :: Effect Unit
test = do
  log $ fullNameResp nameResp
  log $ show $ allCombinations add [1,2,3] [3,4]
  where
    nameResp = fullNameEither (Just "Ibrahim") (Just "Mamdouh") (Just "Ibrahim")