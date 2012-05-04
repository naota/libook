{-# LANGUAGE ViewPatterns #-}

import Control.Monad.Trans.Class (lift)
import System.Environment (getArgs)
import qualified Data.Conduit.List as CL
import Data.Conduit
import qualified Data.Conduit as C
import Data.Maybe (mapMaybe)
import Control.Applicative
import System.Process (rawSystem)
import System.Exit (exitSuccess)
import System.IO (hGetChar, stdin, hSetEcho, hSetBuffering, BufferMode(..))

import Amazon
import Calil

main :: IO ()
main = do
  (email:pass:appkey:libsys) <- getArgs
  runResourceT $ cartBooks email pass
    $= (C.sequence $ CL.take 10)
    $= orderedCheckCond appkey libsys
    $= CL.filter reserveAvailable
    $$ transPipe lift $ CL.mapM_ askReserve
  where reserveAvailable (_, xs) = any isReserveJust xs
        isReserveJust (ReserveOK _ (Just _)) = True
        isReserveJust _ = False

askReserve :: ((String, String), [ReserveState]) -> IO ()
askReserve input@((_, bookname), xs) = do
  putStrLn bookname
  putStrLn "Book available. Reserve it pressing key:"
  putStr . unlines . map g $ commands
  hSetEcho stdin False; hSetBuffering stdin NoBuffering
  cmd <- hGetChar stdin
  hSetEcho stdin True; hSetBuffering stdin LineBuffering
  case lookup cmd commands of
    Just (_, act) -> act
    Nothing -> askReserve input
  where f (ReserveOK sys (Just url)) = Just (sys, openLink url)
        f _ = Nothing
        g (n, (x, _)) = "\t[" ++ n : "]" ++ "\t" ++ x
        avails = mapMaybe f xs
        commands = zip ['1'..] avails ++ basecoms
        basecoms = [ ('i', ("Ignore", putStrLn "Ignored"))
                   , ('Q', ("Quit", exitSuccess))
                   ]

openLink :: String -> IO ()
openLink url = rawSystem "google-chrome" [url] *> return ()
