module Ch7b  where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

newtype CSV = CSV String

class ToCSV a where
  toCSV :: a -> CSV


newtype FullName = FullName String
newtype Age = Age Int
data Occupation = Doctor | Dentist | Lawyer | Unemployed
data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

instance showFullName :: Show FullName where
  show (FullName name) = name

derive instance newtypeFullName :: Newtype FullName _
derive newtype instance eqFullName :: Eq FullName

derive instance newTypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive newtype instance eqAge :: Eq Age

derive instance eqOccupation :: Eq Occupation
derive instance genericOccupation :: Generic Occupation _
instance showOccupation :: Show Occupation where
  show = genericShow



instance toCSVPrson :: ToCSV Person where
  toCSV (Person {name, age, occupation}) = CSV $ show name <> "," <> show age <> "," <> show occupation

derive instance newTypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

derive instance eqPerson :: Eq Person
derive instance geneircPerson :: Generic Person _
instance showPerson :: Show Person where
  show = genericShow


class FromCSV a where
  fromCSV :: CSV -> Maybe a

toOccupation :: String -> Maybe Occupation
toOccupation  = case _ of
  "Doctor" -> Just Doctor
  "Lawyer" -> Just Lawyer
  "Dentist" -> Just Dentist
  "Unemployed" -> Just Unemployed
  _ -> Nothing

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ",") str of 
    [name, age, occupation] -> case fromString age of
      Just age' -> case toOccupation occupation of
        Just occupation' -> Just $ Person {name: FullName name, age: Age age', occupation: occupation'}
        _ -> Nothing        
      
      _ -> Nothing
     
    _ -> Nothing

test :: Effect Unit
test = do
  -- log $ show $ toCSV 
  --   (Person
  --   { name: FullName "Sue Smith"
  --   , age: Age 23
  --   , occupation: Doctor
  --   })
  log $ show $ (toCSV
  (Person
  { name: FullName "Sue Smith"
  , age: Age 23
  , occupation: Doctor
  })) == CSV "Sue Smith,23,Doctor"
  let person = Person { name: FullName "Sue Smith", age: Age 23, occupation: Doctor}
  log $ show $ (toCSV person # fromCSV) == Just person