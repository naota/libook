{-# LANGUAGE OverloadedStrings #-}

module Amazon (cartBooks) where

import Control.Monad.Trans.Class (lift)
import Data.ByteString (isInfixOf)
import Data.ByteString.Char8 (unpack, ByteString, pack)
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (fromJust)
import Network.HTTP.Conduit ( parseUrl, withManager, Response(..), Request(..)
                            , urlEncodedBody, Manager )
import Network.HTTP.Conduit.Browser ( browse, makeRequest, BrowserAction
                                    , getBrowserState, setBrowserState, BrowserState )
import Text.HTML.TagStream (tokenStream, Token'(..), Token, Attr)

type Product = ByteString

cartRequest :: Int -> Request m
cartRequest n = req
  where url = "http://www.amazon.co.jp/gp/cart/view.html?type=saved&page="
        req = fromJust . parseUrl $ url ++ show n

homepageRequest :: Request m 
homepageRequest = req
  where url = "https://www.amazon.co.jp/gp/css/homepage.html"
        req = fromJust $ parseUrl url

signin :: String -> String -> BrowserAction ()
signin email password = do
  Response _ _ bsrc <- makeRequest homepageRequest
  signinRequest <- lift $ bsrc
                   $= tokenStream
                   $= CL.filter isSigninForm
                   $= CL.map formTokenToRequest
                   $= CL.map addFormInfo
                   $$ CL.head
  makeRequest (fromJust signinRequest)
  return ()
  where isSigninForm (TagOpen "form" attrs _) = maybe False isSignin $ lookup "action" attrs
        isSigninForm _ = False
        isSignin url = "/gp/flex/sign-in/select.html" `isInfixOf` url
        actionURL (TagOpen "form" attrs _) = addServ . unpack . fromJust $ lookup "action" attrs
        actionURL _ = error "Expecting form"
        addServ x = "https://www.amazon.co.jp" ++ x
        addFormInfo req = urlEncodedBody params req
        formTokenToRequest = fromJust . parseUrl . actionURL
        params = [ ("useRedirectOnSuccess", "1")
                 , ("path", "/gp/css/homepage.html")
                 , ("action", "sign-in")
                 , ("protocol", "https")
                 , ("email", pack email)
                 , ("password", pack password)
                 ]

filterFormInput :: Resource m => ([Attr] -> Bool) -> Conduit Token m Token
filterFormInput f = conduitState initial push close
  where initial = False
        push False x@(TagOpen "form" attrs _)
          | f attrs = return $ StateProducing True [x]
          | otherwise = return $ StateProducing False []
        push False _ = return $ StateProducing False []
        push True x@(TagOpen "input" _ _) = return $ StateProducing True [x]
        push True x@(TagClose "form") = return $ StateProducing False [x]
        push True _ = return $ StateProducing True []
        close _ = return []

cartBooks :: String -> String -> IO (Source IO Product)
cartBooks email password = withManager $ \manager -> do
  browse manager $ do
    signin email password
    Response _ _ bsrc <- makeRequest $ cartRequest 0
    cartreqs <- lift $ bsrc $= tokenStream
                $= CL.filter isCartToken
                $= CL.map anchorTokenToRequest
                $$ CL.consume
    state <- getBrowserState
    return $ cartPageBooks manager state ((cartRequest 0):cartreqs)
  where isCartToken (TagOpen "a" attrs _) = maybe False isCartLink $ lookup "href" attrs
        isCartToken _ = False
        isCartLink url = "/gp/cart/view.html" `isInfixOf` url
                         && "page=" `isInfixOf` url
        addServ x = "http://www.amazon.co.jp" ++ x
        hrefURL (TagOpen "a" attrs _) = addServ . unpack . fromJust $ lookup "href" attrs
        hrefURL _ = error "Expecting anchor"
        anchorTokenToRequest = fromJust . parseUrl . hrefURL

cartPageBooks :: Manager -> BrowserState -> [Request IO] -> Source IO Product
cartPageBooks manager state reqs = sourceState initial pull
  where initial = (Nothing, reqs)
        pull (Nothing, []) = return $ StateClosed
        pull (Nothing, (next:rest)) = browse manager $ do
          setBrowserState state
          Response _ _ bsrc <- makeRequest next
          lift $ pull (Just (products bsrc), rest)
        pull (Just bsrc, rest) = do
          res <- sourcePull bsrc
          case res of Closed -> pull (Nothing, rest)
                      Open bsrc' val -> return $ StateOpen (Just bsrc', rest) val
        products bsrc = bsrc $= tokenStream
                        $= CL.filter isProduct
                        $= CL.map toProduct
        isProduct (TagOpen "a" attrs _) = maybe False isProductUrl $ lookup "href" attrs
        isProduct _ = False                        
        isProductUrl url = "/gp/product/" `isInfixOf` url
                           && "ref=ox_sc_act_image_" `isInfixOf` url
        toProduct (TagOpen "a" attrs _) = B.takeWhile (/= '/') . B.drop (B.length urlheader) .
                                          fromJust $ lookup "href" attrs
        toProduct _ = error "Expecting product link"
        urlheader = pack "http://www.amazon.co.jp/gp/product/"
