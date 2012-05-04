{-# LANGUAGE OverloadedStrings #-}

module Calil (
  orderedCheckCond
  , checkAPICond
  , libraryAPI
  , ReserveState(..)
  ) where

import Control.Applicative ((<*))
import Control.Monad (forM_, when)
import Control.Monad.Trans.Class (lift)
import Data.Conduit
import Data.Maybe (fromJust, isJust)
import Data.Text ( unpack )
import Data.XML.Types (Event)
import Network.HTTP.Conduit (http, parseUrl, Response(..), withManager, Request(..))
import Text.XML.Stream.Parse ( def, parseBytes, tagNoAttr, ignoreAttrs, content, many
                             , requireAttr, tagName )
import Network.HTTP.Types (renderSimpleQuery)
import Data.ByteString.Char8 (pack)
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime, addUTCTime, UTCTime)

type AppKey = String

type SystemID = String
data Library = Library { systemid :: SystemID
                       , systemname :: String
                       } deriving Show

type Session = String
data CheckAPIResult = CheckContinue Session [BookReserve]
                    | CheckDone [BookReserve]
                    deriving Show

type ISBN = String
type BookReserve = (ISBN, [ReserveState])

type ReserveURL = String
data ReserveState = ReserveOK SystemID (Maybe ReserveURL)
                  | ReserveRunning SystemID
                  | ReserveError SystemID
                  deriving Show

parseSystem :: MonadThrow m => Sink Event m (Maybe ReserveState)
parseSystem = tagName "system" (requireAttr "systemid") $ \sid -> do
  Just st <- tagNoAttr "status" content
  rsv <- tagNoAttr "reserveurl" content
  tagNoAttr "libkeys" $ many parseLibkey
  case unpack st of
    "OK" -> okresult sid $ fmap unpack rsv
    "Cache" -> okresult sid $ fmap unpack rsv
    "Running" -> return . ReserveRunning $ unpack sid
    "Error" -> return . ReserveError $ unpack sid
    _ -> error "Unexpected status"
  where parseLibkey = tagName "libkey" ignoreAttrs $ const content
        okresult sid (Just "") = return $ ReserveOK (unpack sid) Nothing
        okresult sid x = return $ ReserveOK (unpack sid) x

parseBook :: MonadThrow m => Sink Event m (Maybe BookReserve)
parseBook = tagName "book" attr $ \isbn -> do
  sys <- many parseSystem
  return (unpack isbn, sys)
  where attr = requireAttr "isbn" <* ignoreAttrs

parseCheckAPIResult :: MonadThrow m => Sink Event m (Maybe CheckAPIResult)
parseCheckAPIResult = tagNoAttr "result" $ do
  Just session <- tagNoAttr "session" content
  Just cont <- tagNoAttr "continue" content
  Just books <- tagNoAttr "books" $ many parseBook
  return $
    if cont == "0"
    then CheckDone books
    else CheckContinue (unpack session) books

data CheckAPIState = CASInit (Maybe UTCTime) [ISBN]
                   | CASSession UTCTime Session [ISBN]
checkAPICond :: AppKey -> [SystemID] -> Conduit [ISBN] (ResourceT IO) [BookReserve]
checkAPICond appkey libs = nosession Nothing
  where nosession tm = NeedInput (pull tm) close
        pull isbns tm = PipeM (nextcall $ CASInit isbns tm) finish
        nextcall (CASInit tm isbns) = callAPI tm (initQuery isbns) isbns
        nextcall (CASSession tm ses isbns) = callAPI (Just tm) (sesQuery ses) isbns
        finish = undefined
        close = Done Nothing ()
        callAPI tm query isbns = lift $ withManager $ \manager -> do
          cur <- lift getCurrentTime
          when (isJust tm && cur < fromJust tm) $ do
            lift . threadDelay . (`div` (10 ^ 6)) .  fromEnum $ diffUTCTime (fromJust tm) cur
          next <- lift $ fmap (addUTCTime 3) getCurrentTime
          Response _ _ _ bsrc <- http (reqQuery query) manager
          Just res <- bsrc $= parseBytes def $$ parseCheckAPIResult
          return $
            case res of
              CheckContinue ses books -> HaveOutput (insession next ses isbns) finish books
              CheckDone books -> HaveOutput (nosession $ Just next) finish books
        insession tm ses isbns = PipeM (nextcall $ CASSession tm ses isbns) finish
        reqQuery q = basereq { queryString = renderSimpleQuery False q }
        initQuery isbns = [ ("appkey", pack appkey)
                          , ("format", "xml")
                          , ("isbn", isbnlist isbns)
                          , ("systemid", liblist)
                          ]
        sesQuery ses = [ ("appkey", pack appkey)
                       , ("format", "xml")
                       , ("session", pack ses)
                       ]
        isbnlist = pack . concatMap (++ ",")
        liblist = pack $ concatMap (++ ",") libs
        basereq = fromJust $ parseUrl "http://api.calil.jp/check"

withInputWrap :: Monad m => Conduit i m o -> Conduit i m (Maybe i, o)
withInputWrap cond0 = f Nothing cond0
  where clean act = act
        f lastin (HaveOutput next close output) =
          HaveOutput (f lastin next) close (lastin, output)
        f lastin (NeedInput next term) = NeedInput (wrapin next) $ termwrap lastin term
        f _ (Done rest _) = Done rest ()
        f lastin (PipeM next fin) = PipeM (wrappipem lastin next) $ clean fin
        wrapin next input = f (Just input) $ next input
        termwrap ref term = f ref term
        wrappipem lastin next = do
          np <- next
          return $ f lastin np

data OCState = OCInit
             | OCProcess [ISBN]
orderedCheckCond :: AppKey -> [SystemID] -> Pipe [ISBN] (ISBN, [ReserveState]) (ResourceT IO) ()
orderedCheckCond appkey libs = (withInputWrap $ checkAPICond appkey libs) =$= condOrd
  where condOrd = conduitState initial push close
        initial = OCInit
        process isbns apires =
          let (arrived,rest) = consumeArrived apires isbns in
          if rest == []
          then return $ StateProducing OCInit (reverse arrived)
          else return $ StateProducing (OCProcess (reverse rest)) (reverse arrived)
        push OCInit (Just isbns, apires) = process isbns apires
        push OCInit (Nothing, _) = error ""
        push (OCProcess isbns) (_, apires) = process isbns apires
        close _ = return []
        consumeArrived apires isbns' = foldl (f apires) ([], []) isbns'
        f apires (xs, []) isbn =
          case lookup isbn apires of
            Just sysres -> if any isRuning sysres
                           then (xs, [isbn])
                           else ((isbn, sysres):xs, [])
            Nothing -> (xs, [isbn])
        f _ (xs, ys) isbn = (xs, isbn:ys)
        isRuning (ReserveRunning _) = True
        isRuning _ = False
        
parseLibrary :: MonadThrow m => Sink Event m (Maybe Library)
parseLibrary = tagNoAttr "Library" $ do
  Just sid <- tagNoAttr "systemid" content
  Just sname <- tagNoAttr "systemname" content
  forM_ tags $ \x -> tagNoAttr x content
  return $ Library { systemid = unpack sid, systemname = unpack sname }
  where tags = [ "libkey", "libid", "short", "formal", "url_pc", "address", "pref", "city"
               , "post", "tel", "geocode", "category", "image" ]

parseLibraries :: MonadThrow m =>  Sink Event m (Maybe [Library])
parseLibraries = tagNoAttr "Libraries" $ many parseLibrary

libraryAPI :: String -> String -> String -> IO ()
libraryAPI appkey pref city = withManager $ \manager -> do
  Response _ _ _ bsrc <- http req manager
  Just xs <- bsrc $= parseBytes def $$ parseLibraries
  lift $ forM_ xs $ \x -> putStrLn $ systemid x ++ "\t" ++ systemname x
  where req = (fromJust $ parseUrl baseurl) { queryString = renderSimpleQuery False params }
        params = [ ("appkey", pack appkey)
                 , ("pref", pack pref)
                 , ("city", pack city)
                 ]
        baseurl = "http://api.calil.jp/library"
