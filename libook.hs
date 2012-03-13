import System.Environment (getArgs)
import qualified Data.Conduit.List as CL
import Data.Conduit

import Amazon
import Calil

main :: IO ()
main = do
  (email:pass:appkey:_) <- getArgs
  cartSrc <- cartBooks email pass
  isbns <- runResourceT $ cartSrc $$ CL.take 10
  print isbns
  runResourceT $ orderedCheckSrc appkey libsys isbns $$ CL.mapM_ print
  where libsys = [ "Osaka_Ikeda", "Osaka_Toyonaka", "Univ_Osaka" ]
