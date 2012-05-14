{-# LANGUAGE ViewPatterns #-}

import Control.Concurrent (forkIO)
import qualified Control.Exception as CE
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import System.Environment (getArgs)
import Data.Either.Utils (forceEither)
import qualified Data.Conduit.List as CL
import Data.Conduit
import qualified Data.Conduit as C
import Data.ConfigFile (get, emptyCP, readfile)
import Data.IORef (newIORef, readIORef, modifyIORef)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Control.Applicative
import System.Process (rawSystem)
import System.IO (hGetChar, stdin, hSetEcho, hSetBuffering, BufferMode(..))

import Amazon
import Calil
import Booklog

main :: IO ()
main = do
  (config:target) <- getArgs
  (email, pass, appkey, bookloguser, libsys, cacheFile) <- readConfig config
  cache <- CE.catch (fmap read $ readFile cacheFile)
           (\e -> do
               putStrLn $ show (e :: CE.IOException)
               return M.empty)
  CE.bracket (newIORef cache)
    (savecache cacheFile)
    (\cacheref -> mainwork email pass appkey bookloguser target libsys cacheref)
  where registerbook ref bdata@(isbn, _) = do
          modifyIORef ref (isbn :)
          return bdata
        mainwork email pass appkey bookloguser target libsys cacheref = do
          bookref <- newIORef []
          continue <- runResourceT $ cartBooks email pass
              $= (transPipe lift . CL.mapM $ registerbook bookref)
              $= (C.sequence $ CL.take 10)
              $= (transPipe lift $ orderedCheckCond appkey libsys cacheref)
              $= CL.filter (reserveAvailable target)
              $$ transPipe lift askReserveSink
          when continue $ do
            books <- readIORef bookref
            runResourceT $ wantReadBooks bookloguser
              $= (C.sequence $ CL.take 10)
              $= (transPipe lift $ orderedCheckCond appkey libsys cacheref)
              $$ transPipe lift $ booklogBooksSink books target libsys
            return ()
        savecache file ref = do
          newcache <- readIORef ref
          writeFile file $ show newcache

readConfig :: FilePath -> IO (String, String, String, String, [String], FilePath)
readConfig file = do
  val <- readfile emptyCP file
  let cp = forceEither val
      f x y = forceEither $ get cp x y
      email = f "Amazon" "email"
      pass = f "Amazon" "password"
      appkey = f "Calil" "appkey"
      bookloguser = f "Booklog" "user"
      cacheFile = f "Cache" "file"
      libsys = f "Calil" "system"
  return (email, pass, appkey, bookloguser, libsys, cacheFile)

askReserveSink :: Sink (BookData, [ReserveState]) IO Bool
askReserveSink = wait
  where wait = NeedInput pull close
        close = Done Nothing True
        pull input = PipeM (askLoop input wait) $ return True

askLoop :: ((t, String), [ReserveState]) -> Pipe i o m Bool -> IO (Pipe i o m Bool)
askLoop input@((_, bookname), xs) continue = do
          putStrLn bookname
          putStrLn "本見つかりました。予約するなりスルーするなりしてください。"
          putStr . unlines . map g $ commands
          hSetEcho stdin False; hSetBuffering stdin NoBuffering
          cmd <- hGetChar stdin
          hSetEcho stdin True; hSetBuffering stdin LineBuffering
          case lookup cmd commands of
            Just (_, act) -> act
            Nothing -> askLoop input continue
  where f (ReserveOK sys (Just url)) = Just (sys, openLink url)
        f _ = Nothing
        g (n, (x, _)) = "\t[" ++ n : "]" ++ "\t" ++ x
        avails = mapMaybe f xs
        commands = zip ['1'..] avails ++ basecoms
        basecoms = [ ('i', ("Ignore", putStrLn "スルーしました" *> return continue))
                   , ('Q', ("Quit", return $ Done Nothing False))
                   ]
        openLink url = openBrowser url *> return continue

openBrowser :: String -> IO ()
openBrowser url = (forkIO $ (rawSystem "firefox" [url] >> return ())) >> return ()

reserveAvailable :: [String] -> (t, [ReserveState]) -> Bool
reserveAvailable target (_, xs) = any isReserveJust xs
  where isReserveJust (ReserveOK sys (Just _)) = elem sys target
        isReserveJust _ = False

booklogBooksSink :: [ISBN] -> [String] -> [String] -> Sink (BookData, [ReserveState]) IO Bool
booklogBooksSink books target libsys = wait
  where wait = NeedInput pull close
        pull input = PipeM (check input) $ return True
        close = Done Nothing False
        check input@(reserveAvailable target -> True) = askLoop input wait
        check input@(reserveAvailable libsys -> True) = do
          let ((_, title), _) = input
          putStrLn $ "Skip in untargeted library: " ++ title
          return wait
        check ((flip elem books -> True, title), _) = do
          putStrLn $ "Skip in Amazon: " ++ title
          return wait
        check ((isbn, title), _) = do
          putStrLn title
          putStrLn "どの図書館にも見つからないAmazon未登録の本です。 Amazonページを開きます。"
          openBrowser $ url isbn
          return wait
        url isbn = "http://www.amazon.co.jp/gp/product/" ++ isbn
