module Ch9  where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, one, show, ($), (&&), (==))

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup a <= Monoid a where
  mempty :: a

class Monoid a <= Group a where
  ginverse :: a -> a
  
class Semigroup g <= Commutative g
-- AndBool type
data AndBool = AFalse | ATrue
derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
  show = genericShow

instance semigroupAndBool :: Semigroup AndBool where
  append ATrue ATrue = ATrue
  append _ _ = AFalse

instance monoidAndBool :: Monoid AndBool where
  mempty = ATrue

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do
  log "Verifying AndBool Semigroup Laws (1 test)"
  log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue) 

verifyAndBoolMonoid :: Effect Unit
verifyAndBoolMonoid = do
  log "Verifying AndBool Monoid Laws (2 tests)"
  log $ show $ ((ATrue <> mempty) == ATrue) && ((mempty <> ATrue) == ATrue)
  log $ show $ ((AFalse <> mempty) == AFalse) && ((mempty <> AFalse) == AFalse)
-- End AndBool type

-- OrBool type
data OrBool = OFalse | OTrue

derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _
instance showOrBool :: Show OrBool where
  show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying OrBool Semigroup Laws (1 test)"
  log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue) 

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do
  log "Verifying OrBool Monoid Laws (2 tests)"
  log $ show $ mempty <> OTrue == OTrue <> mempty && OTrue <> mempty == OTrue
  log $ show $ mempty <> OFalse == OFalse <> mempty && OFalse <> mempty == OFalse

-- End OrBool

-- Mod4 type
data Mod4 =  Zero | One | Two | Three
derive instance eqMod4 :: Eq Mod4

instance semigroupMod4 :: Semigroup Mod4 where
  append Zero x = x
  append x Zero = x

  append One One = Two
  append One Two = Three
  append One Three = Zero
  
  append Two One = Three
  append Two Two = Zero
  append Two Three = One
  
  append Three One = Zero  
  append Three Two = One
  append Three Three = Two

instance monoidMod4 :: Monoid Mod4 where
  mempty = Zero

instance commutativeMod4 :: Commutative Mod4

instance groupMod4 :: Group Mod4 where
  ginverse Zero = Zero
  ginverse One = Three
  ginverse Two = Two
  ginverse Three = One

verifyMod4Semigroup :: Effect Unit
verifyMod4Semigroup = do
  log "Verifying Mod4 Semigroup Laws (1 test)"
  log $ show $ (One <> Two) <> Three == One <> (Two <> Three)

verifyMod4Monoid :: Effect Unit
verifyMod4Monoid = do
  log "Verifying Mod4 Monoid Laws (4 tests)"
  log $ show $ Zero <> mempty == Zero && mempty <> Zero == Zero
  log $ show $ One <> mempty == One && mempty <> One == One
  log $ show $ Two <> mempty == Two && mempty <> Two == Two
  log $ show $ Three <> mempty == Three && mempty <> Three == Three

-- End Mod4 type

-- First type
newtype First a = First (Maybe a)

instance semigroupFirst :: Semigroup  (First a) where
  append (First Nothing) last = last
  append first _ = first

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing

instance showFirst :: Show a => Show (First a) where
  show (First x) = show x

-- End First type

newtype Last a = Last (Maybe a)

instance semigroupLast :: Semigroup  (Last a) where
  append first (Last Nothing) = first
  append _ last = last

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing

instance showLast :: Show a => Show (Last a) where
  show (Last x) = show x

test :: Effect Unit
test = do
    log $ show $ First Nothing <> First (Just 77) 
    log $ show $ Last (Just 1) <> Last (Just 99) 