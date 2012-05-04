{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Amazon (cartBooks) where

import Control.Monad.Trans.Class (lift)
import Data.ByteString (isInfixOf)
import Data.ByteString.Char8 (unpack, pack)
import qualified Data.ByteString.Char8 as B
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Maybe (fromJust)
import Network.HTTP.Conduit ( parseUrl, withManager, Response(..), Request(..)
                            , urlEncodedBody )
import Network.HTTP.Conduit.Browser ( browse, makeRequest, BrowserAction
                                    , getBrowserState, setBrowserState )
import Text.HTML.TagStream (tokenStream, Token'(..))

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
  Response _ _ _ bsrc <- makeRequest homepageRequest
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

cartBooks :: String -> String -> Source (ResourceT IO) String
cartBooks email password = sourceStateIO initial clean pull
  where initial = withManager $ \manager -> do
          browse manager $ do
            signin email password
            Response _ _ _ bsrc <- makeRequest $ cartRequest 0
            cartreqs <- lift $ bsrc $= tokenStream
                        $= CL.filter isCartToken
                        $= CL.map anchorTokenToRequest
                        $$ CL.consume
            state <- getBrowserState
            return ([], manager, state, reverse $ (cartRequest 0):cartreqs)
        clean _ = return ()
        pull ([], _, _, []) = return StateClosed
        pull ([], manager, state, (next:rest)) = browse manager $ do
          setBrowserState state
          Response _ _ _ bsrc <- makeRequest next
          res <- lift $ products bsrc $$ CL.consume
          lift $ pull (reverse res, manager, state, rest)
        pull (p:ps, manager, state, rest) = return $ StateOpen (ps, manager, state, rest) p
        isCartToken (TagOpen "a" attrs _) = maybe False isCartLink $ lookup "href" attrs
        isCartToken _ = False
        anchorTokenToRequest = fromJust . parseUrl . hrefURL
        products bsrc = bsrc $= tokenStream
                        $= CL.filter isProduct
                        $= CL.map toProduct
        isProduct (TagOpen "a" attrs _) = maybe False isProductUrl $ lookup "href" attrs
        isProduct _ = False
        isCartLink url = "/gp/cart/view.html" `isInfixOf` url &&
                         "page=" `isInfixOf` url
        hrefURL (TagOpen "a" attrs _) = addServ . unpack . fromJust $ lookup "href" attrs
        hrefURL _ = error "Expecting anchor"
        toProduct (TagOpen "a" attrs _) = unpack $ B.takeWhile (/= '/') .
                                          B.drop (B.length urlheader) .
                                          fromJust $ lookup "href" attrs
        toProduct _ = error "Expecting product link"
        urlheader = pack "http://www.amazon.co.jp/gp/product/"
        isProductUrl url = "/gp/product/" `isInfixOf` url &&
                           "ref=ox_sc_act_image_" `isInfixOf` url
        addServ x = "http://www.amazon.co.jp" ++ x
