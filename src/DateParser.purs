module DateParser where

newtype Year
  = Year Int

newtype Month
  = Month Int

newtype Day
  = Day Int

data DateFormat
  = YearFirstFormat
  | MonthFirstFormat

type DateParts
  = { year :: Year
    , month :: Month
    , day :: Day
    , format :: DateFormat
    }
