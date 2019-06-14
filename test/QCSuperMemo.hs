module QCSuperMemo (main) where

import SuperMemo
import Data.Time.Clock
import Data.Time.Calendar
import Test.QuickCheck
import Test.QuickCheck.Instances.Time

arbitraryLD :: Gen LearningData
arbitraryLD = do
    steps <- suchThat arbitrary (>=0) :: Gen Int
    ef <- suchThat arbitrary (>=1.3) :: Gen Float
    dtime <- arbitrary
    lastReview <- arbitrary
    return $ LD steps ef lastReview dtime

instance Arbitrary LearningData where
    arbitrary = arbitraryLD

--put a good test of EF here


-- If updating with q=0, steps goes to 0 and ef is unchanged
testUpdateZero :: LearningData -> UTCTime -> Bool
testUpdateZero ld t = case update ld 0 t of
    (LD steps ef lr dt) -> steps==0 && ef==(getEf ld)


-- Steps always increases by 1, unless q<3
testUpdateSteps :: LearningData -> Float -> UTCTime -> Bool
testUpdateSteps ld q t = (getSteps ld' == getSteps ld +1) || q<3
    where ld' = update ld q t

-- review urgency is zero if review is in the future
urgencyAndNextReview :: LearningData -> UTCTime -> Bool
urgencyAndNextReview ld today = nextReview ld <= today || reviewUrgency ld today == 0

urgency :: LearningData -> UTCTime -> Bool
urgency ld today = reviewUrgency ld today >= 0


main = do
    quickCheck testUpdateZero
    quickCheck testUpdateSteps
    quickCheck urgency
    quickCheck urgencyAndNextReview