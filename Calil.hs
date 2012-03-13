{-# LANGUAGE OverloadedStrings #-}

module Calil (orderedCheckSrc, libraryAPI, ReserveState(..)) where

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
import Data.ByteString.Char8 (pack, ByteString)
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

parseSystem :: Sink Event IO (Maybe ReserveState)
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

parseBook :: Sink Event IO (Maybe BookReserve)
parseBook = tagName "book" attr $ \isbn -> do
  sys <- many parseSystem
  return (unpack isbn, sys)
  where attr = requireAttr "isbn" <* ignoreAttrs

parseCheckAPIResult :: Sink Event IO (Maybe CheckAPIResult)
parseCheckAPIResult = tagNoAttr "result" $ do
  Just session <- tagNoAttr "session" content
  Just cont <- tagNoAttr "continue" content
  Just books <- tagNoAttr "books" $ many parseBook
  if cont == "0"
    then return $ CheckDone books
    else return $ CheckContinue (unpack session) books

data CheckAPIState = CASInit (Source IO [ISBN]) (Maybe UTCTime)
                   | CASSession (Source IO [ISBN]) (Maybe UTCTime) ByteString [ISBN]
checkAPISrc :: AppKey -> [SystemID] -> Source IO [ISBN] -> Source IO ([ISBN], [BookReserve])
checkAPISrc appkey libs src = sourceStateIO initial clean pull
  where initial = return $ CASInit src Nothing
        clean _ = return ()
        callAPI tm query src' isbns = withManager $ \manager -> do
          cur <- lift getCurrentTime
          when (isJust tm && cur < fromJust tm) $ do
            lift . threadDelay . (`div` (10 ^ 6)) .  fromEnum $ diffUTCTime (fromJust tm) cur
          next <- lift $ fmap (addUTCTime 3) getCurrentTime
          Response _ _ bsrc <- http (reqQuery query) manager
          Just res <- bsrc $= parseBytes def $$ parseCheckAPIResult
          case res of
            CheckContinue ses xs ->
              return $ StateOpen (CASSession src' (Just next) (pack ses) isbns) (isbns,xs)
            CheckDone xs ->
              return $ StateOpen (CASInit src' (Just next)) (isbns, xs)
        pull (CASInit src' tm) = do
          res <- runResourceT $ sourcePull src'
          case res of
            Open src'' isbns -> callAPI tm (initQuery isbns) src'' isbns
            Closed -> return $ StateClosed
        pull (CASSession src' tm ses isbns) = callAPI tm (sesQuery ses) src' isbns
        reqQuery q = basereq { queryString = renderSimpleQuery False q }
        initQuery isbns = [ ("appkey", pack appkey)
                          , ("format", "xml")
                          , ("isbn", isbnlist isbns)
                          , ("systemid", liblist)
                          ]
        sesQuery ses = [ ("appkey", pack appkey)
                       , ("format", "xml")
                       , ("session", ses)
                       ]
        isbnlist = pack . concatMap (++ ",")
        liblist = pack $ concatMap (++ ",") libs
        basereq = fromJust $ parseUrl "http://api.calil.jp/check"

data OCState = OCInit
             | OCProcess [ISBN]
orderedCheckSrc :: AppKey -> [SystemID] -> Source IO [ISBN] -> Source IO BookReserve
orderedCheckSrc appkey libs src = checkAPISrc appkey libs src $= condOrd
  where condOrd :: Conduit ([ISBN], [BookReserve]) IO BookReserve
        condOrd = conduitState initial push close
        initial = OCInit
        process isbns apires =
          let (arrived,rest) = consumeArrived apires isbns in
          if rest == []
          then return $ StateProducing OCInit (reverse arrived)
          else return $ StateProducing (OCProcess (reverse rest)) (reverse arrived)
        push OCInit (isbns, apires) = process isbns apires
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
        
parseLibrary :: Sink Event IO (Maybe Library)
parseLibrary = tagNoAttr "Library" $ do
  Just sid <- tagNoAttr "systemid" content
  Just sname <- tagNoAttr "systemname" content
  forM_ tags $ \x -> tagNoAttr x content
  return $ Library { systemid = unpack sid, systemname = unpack sname }
  where tags = [ "libkey", "libid", "short", "formal", "url_pc", "address", "pref", "city"
               , "post", "tel", "geocode", "category", "image" ]

parseLibraries :: Sink Event IO (Maybe [Library])
parseLibraries = tagNoAttr "Libraries" $ many parseLibrary

libraryAPI :: String -> String -> String -> IO ()
libraryAPI appkey pref city = withManager $ \manager -> do
  Response _ _ bsrc <- http req manager
  Just xs <- bsrc $= parseBytes def $$ parseLibraries
  lift $ forM_ xs $ \x -> putStrLn $ systemid x ++ "\t" ++ systemname x
  where req = (fromJust $ parseUrl baseurl) { queryString = renderSimpleQuery False params }
        params = [ ("appkey", pack appkey)
                 , ("pref", pack pref)
                 , ("city", pack city)
                 ]
        baseurl = "http://api.calil.jp/library"
