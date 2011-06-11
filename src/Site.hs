{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text.Encoding as T
import           Snap.Extension.Heist
import           Snap.Extension.Timer
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import           Generator

import           Control.Monad.Reader
import           Control.Monad.Random (evalRandIO)


index :: Application ()
index = ifTop $ do
    model <- asks modelState
    trackName <- liftIO . evalRandIO $ inventName model
    heistLocal (bindString "track-name" trackName) $ render "index"

site :: Application ()
site = route [ ("/",            index)
             ]
       <|> serveDirectory "resources/static"
