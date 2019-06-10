import Database
import Data.Time.Clock
import Data.UUID
import Data.UUID.V4
import SuperMemo

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

main :: IO ()
main = do
    d <- buildDatabase
    putStrLn $ show d
    writeDatabase d "base"
    d' <-fromJust <$> readDatabase "base"
    putStrLn $ show d' -- should be identical
    putStrLn $ show (d == d') -- should be "True"
    today <- getCurrentTime
    putStrLn $ show $ toReview today d --should return those entries that are old enough.
    return ()


--Todo: profile memory usage for million-card databases
--Add tests for supermemo stuff.