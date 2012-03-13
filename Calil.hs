{-# LANGUAGE OverloadedStrings #-}

module Calil (checkAPISrc, libraryAPI) where

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
import Data.Time (getCurrentTime, diffUTCTime, addUTCTime)

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

checkAPISrc :: AppKey -> [SystemID] -> [ISBN] -> Source IO [BookReserve]
checkAPISrc appkey libs isbns = sourceStateIO initial clean pull
  where initial = return (Nothing, Nothing)
        clean _ = return ()
        callAPI tm query = withManager $ \manager -> do
          cur <- lift getCurrentTime
          when (isJust tm && cur < fromJust tm) $ do
            lift . threadDelay . (`div` (10 ^ 6)) .  fromEnum $ diffUTCTime (fromJust tm) cur
          next <- lift $ fmap (addUTCTime 3) getCurrentTime
          Response _ _ bsrc <- http (reqQuery query) manager
          Just res <- bsrc $= parseBytes def $$ parseCheckAPIResult
          case res of
            CheckContinue ses xs -> return $ StateOpen (Just . Just $ pack ses, Just next) xs
            CheckDone xs -> return $ StateOpen (Just Nothing, Just next) xs
        pull (Nothing, _) = callAPI Nothing initQuery
        pull (Just Nothing, _) = return $ StateClosed
        pull (Just (Just ses), tm) = callAPI tm $ sesQuery ses
        reqQuery q = basereq { queryString = renderSimpleQuery False q }
        initQuery = [ ("appkey", pack appkey)
                    , ("format", "xml")
                    , ("isbn", isbnlist)
                    , ("systemid", liblist)
                    ]
        sesQuery ses = [ ("appkey", pack appkey)
                       , ("format", "xml")
                       , ("session", ses)
                       ]
        isbnlist = pack $ concatMap (++ ",") isbns
        liblist = pack $ concatMap (++ ",") libs
        basereq = fromJust $ parseUrl "http://api.calil.jp/check"

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
