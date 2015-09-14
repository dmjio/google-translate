google-translate
====================
![Hackage](https://img.shields.io/hackage/v/google-translate.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/google-translate.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)
![Build Status](https://img.shields.io/circleci/project/dmjio/stripe-haskell.svg)

High-level, up-to-date bindings to the Google Translate API.
```haskell
import Control.Monad
import qualified Data.Text.IO as T
import Data.Maybe
import Web.Google.Translate                                                                                             
                                                                                                                                                                                  
main :: IO ()
main = do
  Right TranslationResponse { translations = xs } <-
    translate (Key "<API-Key>") (Just srcLang) trgLang (Body "Hello")
  forM_ xs $ \Translation { translatedText = TranslatedText txt } ->
    T.putStrLn txt
  where
    srcLang = Source English
    trgLang = Target Russian
    
-- >>> Здравствуйте    
```    