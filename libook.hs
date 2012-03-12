import System.Environment (getArgs)
import qualified Data.Conduit.List as CL
import Data.Conduit

import Amazon

main :: IO ()
main = do
  (email:pass:_) <- getArgs
  cartSrc <- cartBooks email pass
  runResourceT $ cartSrc $= CL.isolate 10 $$ CL.mapM_ print
