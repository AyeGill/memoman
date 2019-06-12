{-# LANGUAGE OverloadedStrings
           , DeriveGeneric
           , DefaultSignatures
           , StandaloneDeriving #-}

module SuperMemo
    ( someFunc
    , newEF
    , update
    , LearningData (LD)
    , module T
    , nextReview
    , mkLearningData
    ) where

import qualified Data.Time.Clock as T (UTCTime
                                     , NominalDiffTime
                                     , nominalDay
                                     , getCurrentTime
                                     , diffUTCTime
                                     , addUTCTime)
import Data.Serialize
import GHC.Generics
import GHC.Exts
import Data.Time.Clock.Serialize

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data LearningData = LD  {steps :: !Int, -- Times reviewed since last reset
                         ef :: !Float,  -- Easiness factor
                         lastReviewed :: !T.UTCTime,
                         reviewDelay :: !T.NominalDiffTime} deriving (Show, Eq, Generic)
instance Serialize LearningData

mkLearningData :: T.UTCTime -> LearningData
mkLearningData today = LD 0 2.5 today 0
--at first, "review" immediately.

nextReview (LD _ _ last delay) = T.addUTCTime delay last


--Some convenience functions for time.
m :: Float -> T.NominalDiffTime -> T.NominalDiffTime
m x t = (fromRational $ toRational x) * t

days :: Float -> T.NominalDiffTime
days x = x `m` T.nominalDay


{-
q is quality of response, 0-5 scale.
5 - perfect response
4 - correct response after a hesitation
3 - correct response recalled with serious difficulty
2 - incorrect response; where the correct one seemed easy to recall
1 - incorrect response; the correct one remembered
0 - complete blackout.-}

newEF :: Float -> Float -> Float 
newEF ef q = max 1.3 $ ef + delta
    where delta = if q < 3
                  then 0 -- if we "failed", just keep EF where it is.
                  else 0.1 - (5-q)*(0.08+(5-q)*0.02)

update :: LearningData -> Float -> T.UTCTime -> LearningData
update (LD steps ef last reviewDelay) q today = LD newSteps newEf today newDelay
    where fail = (q < 2)
          newSteps = if fail then 0 else (steps+1)
          newEf = newEF ef q
          newDelay = case newSteps of 
                                    0 -> 0
                                    1 -> T.nominalDay
                                    2 -> 6*T.nominalDay
                                    _ -> (m newEf reviewDelay)


reviewUrgency :: LearningData -> T.UTCTime -> Float
reviewUrgency (LD _ _ last reviewDelay) today = 
    fromRational $ toRational $ max 0 $ (T.diffUTCTime today last) - reviewDelay
    -- If time passed since last review > delay, just return 0.
    -- Else return difference (more = more urgent to review.)
    -- Return as float (seconds, I think) for convenience