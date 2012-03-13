{-# LANGUAGE ViewPatterns #-}

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

($=$) :: t1 -> (t1 -> t) -> t
($=$) = flip ($)
infixl 1 $=$

main :: IO ()
main = do
  (email:pass:appkey:libsys) <- getArgs
  cartSrc <- cartBooks email pass
  runResourceT $ cartSrc
    $= (C.sequence $ CL.take 10)
    $=$ orderedCheckSrc appkey libsys
    $= CL.filter reserveAvailable
    $$ CL.mapM_ askReserve
  where reserveAvailable (_, xs) = any isReserveJust xs
        isReserveJust (ReserveOK _ (Just _)) = True
        isReserveJust _ = False

askReserve :: (t, [ReserveState]) -> IO ()
askReserve (isbn, xs) = do
  putStrLn "Book available. Reserve it pressing key:"
  putStr . unlines . map g $ commands
  hSetEcho stdin False; hSetBuffering stdin NoBuffering
  cmd <- hGetChar stdin
  hSetEcho stdin True; hSetBuffering stdin LineBuffering
  case lookup cmd commands of
    Just (_, act) -> act
    Nothing -> askReserve (isbn, xs)
  where f (ReserveOK sys (Just url)) = Just (sys, openLink url)
        f _ = Nothing
        g (n, (x, _)) = "\t[" ++ n : "]" ++ "\t" ++ x
        avails = mapMaybe f xs
        commands = zip ['1'..] avails ++ basecoms
        basecoms = [ ('i', ("Ignore", putStrLn "Ignored"))
                   , ('q', ("Quit", exitSuccess))
                   ]

openLink :: String -> IO ()
openLink url = rawSystem "google-chrome" [url] *> return ()
