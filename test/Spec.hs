import Database
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4
import SuperMemo
import SimpleCard

fromJust (Just x) = x

mkDataYesterday :: IO LearningData
mkDataYesterday = do
    today <- (\x -> addUTCTime (-1 * nominalDay) x)  <$> getCurrentTime
    return $ mkLearningData today

mkData :: IO LearningData
mkData = do
    today <- getCurrentTime
    return $ mkLearningData today

buildDatabase :: IO Database
buildDatabase = do
    ids <- sequence $ replicate 100 $ nextRandom
    let filePaths = cycle ["foo/bar.tex"
                   , "foo/baz.tex"
                   , "quux/derp.html"
                   , "foo/burp.png"]
    dats <- sequence $ replicate 4 $ mkData
    ydats <- sequence $ replicate 4 $ mkDataYesterday

    let entries = zipWith3 E ids filePaths (dats ++ ydats)
    return $ mkDatabase entries

dbTest :: IO ()
dbTest = do
    d <- buildDatabase
    putStrLn $ show d
    writeDatabase d "testfiles/base"
    d' <-fromJust <$> readDatabase "testfiles/base"
    putStrLn $ show d' -- should be identical
    putStrLn $ show (d == d') -- should be "True"
    today <- getCurrentTime
    putStrLn $ show $ toReview today d --should return those entries that are old enough.
    return ()

scTest :: IO ()
scTest = do
    db <- addCards (mkDatabase []) "testfiles/testfile"
    print db
    today <- getCurrentTime
    let cards = toReview (addUTCTime nominalDay today) db
    print cards
    print "\n"
    texts <- sequence $ map findText cards
    print texts

main = scTest
--Todo: profile memory usage for million-card databases
--Add tests for supermemo stuff.