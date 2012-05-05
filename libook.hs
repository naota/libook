{-# LANGUAGE ViewPatterns #-}

import qualified Control.Exception as CE
import Control.Monad.Trans.Class (lift)
import System.Environment (getArgs)
import qualified Data.Conduit.List as CL
import Data.Conduit
import qualified Data.Conduit as C
import Data.IORef (newIORef, readIORef)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Control.Applicative
import System.Process (rawSystem)
import System.IO (hGetChar, stdin, hSetEcho, hSetBuffering, BufferMode(..))

import Amazon
import Calil

main :: IO ()
main = do
  (email:pass:appkey:libsys) <- getArgs
  cache <- CE.catch (fmap read $ readFile cacheFile)
           (\e -> do
               putStrLn $ show (e :: CE.IOException)
               return M.empty)
  cacheref <- newIORef cache
  continue <- runResourceT $ cartBooks email pass
              $= (C.sequence $ CL.take 10)
              $= (transPipe lift $ orderedCheckCond appkey libsys cacheref)
              $= CL.filter reserveAvailable
              $$ askReserveSink
  newcache <- readIORef cacheref
  writeFile cacheFile $ show newcache
  where reserveAvailable (_, xs) = any isReserveJust xs
        isReserveJust (ReserveOK _ (Just _)) = True
        isReserveJust _ = False
        cacheFile = "libook.cache"

askReserveSink :: Sink ((String, String), [ReserveState]) IO Bool
askReserveSink = NeedInput pull close
  where close = Done Nothing True
        pull input = PipeM (loop input) $ return True
        loop input@((_, bookname), xs) = do
          putStrLn bookname
          putStrLn "Book available. Reserve it pressing key:"
          putStr . unlines . map g $ commands xs
          hSetEcho stdin False; hSetBuffering stdin NoBuffering
          cmd <- hGetChar stdin
          hSetEcho stdin True; hSetBuffering stdin LineBuffering
          case lookup cmd $ commands xs of
            Just (_, act) -> act
            Nothing -> loop input
        f (ReserveOK sys (Just url)) = Just (sys, openLink url)
        f _ = Nothing
        g (n, (x, _)) = "\t[" ++ n : "]" ++ "\t" ++ x
        avails xs = mapMaybe f xs
        commands xs = zip ['1'..] (avails xs) ++ basecoms
        basecoms = [ ('i', ("Ignore", putStrLn "Ignored" *> (return $ NeedInput pull close)))
                   , ('Q', ("Quit", return $ Done Nothing False))
                   ]
        openLink url = rawSystem "google-chrome" [url] *> (return $ NeedInput pull close)
