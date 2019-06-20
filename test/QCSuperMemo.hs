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
testUpdateZero :: LearningData -> UTCTime -> Property
testUpdateZero ld t = case update ld 0 t of
    (LD steps ef lr dt) -> (steps===0) .&&. (ef===(getEf ld))

    

-- Steps always increases by 1, assuming that q>=3
testUpdateSteps :: LearningData -> Float -> UTCTime -> Property
testUpdateSteps ld q t = q >= 3 ==> (getSteps ld' == getSteps ld +1)
    where ld' = update ld q t

-- review urgency is zero if review is in the future
urgencyAndNextReview :: LearningData -> UTCTime -> Bool
urgencyAndNextReview ld today = nextReview ld <= today || reviewUrgency ld today == 0

urgency :: LearningData -> UTCTime -> Bool
urgency ld today = reviewUrgency ld today >= 0


main = do
    putStrLn "Testing: Null updates"
    quickCheck testUpdateZero
    putStrLn "Testing: Update steps"
    quickCheck testUpdateSteps
    putStrLn "Testing: Urgency"
    quickCheck urgency
    putStrLn "Testing: Urgency and next review."
    quickCheck urgencyAndNextReview