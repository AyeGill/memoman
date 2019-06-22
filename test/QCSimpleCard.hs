{-# LANGUAGE OverloadedStrings #-}

module QCSimpleCard (main) where
import Test.QuickCheck
import Test.QuickCheck.Instances.Text
import SimpleCard
import Data.Text as T hiding (any)


testSplit :: T.Text -> T.Text -> Property
testSplit a b =
    T.empty `notElem` T.lines a ==> 
    splitCard (T.unlines [T.strip a, "\n", T.strip b]) == (T.strip a, T.strip b)

main = do
    putStrLn "Testing: Split"
    quickCheck testSplit