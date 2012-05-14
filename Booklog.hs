{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Booklog (wantReadBooks) where

import Control.Monad.Trans.Class (lift)
import Data.ByteString (isInfixOf)
import Data.ByteString.Char8 (unpack)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text.ICU.Convert (open, toUnicode)
import Network.HTTP.Conduit ( withManager, parseUrl, Response(..), http )
import Text.HTML.TagStream (tokenStream, Token'(..))

type ISBN = String
type BookTitle = String
type UserName = String

wantReadBooks :: UserName -> Source (ResourceT IO) (ISBN, BookTitle)
wantReadBooks user = readPage (1 :: Int)
  where pageread n = lift . withManager $ \manager -> do
          Response _ _ _ bsrc <- http (request n) manager
          codec <- lift $ open "UTF-8" Nothing
          bs <- bsrc $= tokenStream
            $= CL.filter isBookToken
            $= CL.map (toBookData codec)
            $$ CL.consume
          return $
            if null bs
            then Done Nothing ()
            else haveBooks n bs
        url n = "http://booklog.jp/users/" ++ user ++
                "/?display=list&category_id=0&status=1&rank=0&sort=sort_asc&page=" ++ (show n)
        request = fromJust . parseUrl . url
        finish = return ()
        isBookToken (TagOpen "a" attrs _) = maybe False isBookUrl $ lookup "href" attrs
        isBookToken _ = False
        isBookUrl = ("/archives/1/" `isInfixOf`)
        toBookData codec (TagOpen "a" attrs _) = ( toISBN $ decode codec "href" attrs
                                                 , decode codec "title" attrs)
        toBookData _ _ = error ""
        decode codec attrname = T.unpack . toUnicode codec . fromJust . lookup attrname
        haveBooks n (b:bs) = HaveOutput (haveBooks n bs) finish b
        haveBooks n [] = readPage $ n + 1
        readPage n = PipeM (pageread n) finish
        toISBN = reverse . takeWhile (/= '/') . reverse

