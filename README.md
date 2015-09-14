google-translate
===================
High-level, up-to-date bindings to the Google Translate API.
```haskell
import Control.Monad
import qualified Data.Text.IO as T
import Data.Maybe
import Web.Google.Translate                                                                                             
                                                                                                                                                                                  
main :: IO ()
main = do
  Right TranslationResponse { translations = xs } <-
    translate (Key "<API-Key>") (Just srcLang) trgLang (Body "nice to meet you")
  forM_ xs $ \Translation { translatedText = TranslatedText txt } ->
    T.putStrLn txt
  where
    srcLang = Source English
    trgLang = Target Russian
    
-- >>> Здравствуйте    
```    