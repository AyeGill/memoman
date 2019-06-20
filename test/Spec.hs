import qualified QCSuperMemo as Sm (main)
import qualified QCSimpleCard as Sc (main)
import qualified QCDatabase as Db (main)
import Shelly

main = do
    Sm.main
    Sc.main
    Db.main
