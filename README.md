google-translate
====================
![Hackage](https://img.shields.io/hackage/v/google-translate.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/google-translate.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
![Build Status](https://img.shields.io/circleci/project/dmjio/google-translate.svg)

High-level, up-to-date bindings to the Google Translate API.
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Web.Google.Translate

main :: IO ()
main = do
  Right TranslationResponse { translations = xs } <-
    newManager tlsManagerSettings >>= \mgr ->
    translate mgr (Key "<API-Key>") (Just srcLang) trgLang (Body "Hello")
  forM_ xs $ \Translation { translatedText = TranslatedText txt } ->
    T.putStrLn txt
  where
    srcLang = Source English
    trgLang = Target Russian

-- >>> Здравствуйте
```
