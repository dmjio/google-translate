{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
------------------------------------------------------------------------------
module Web.Google.Translate
       ( -- * Functions
         detect
       , getLanguages
       , translate
         -- * Types
       , Key                 (..)
       , Source              (..)
       , Target              (..)
       , Body                (..)
       , Lang                (..)
       , Confidence          (..)
       , IsReliable          (..)
       , TranslatedText      (..)
       , TranslationResponse (..)
       , Translation         (..)
       , DetectionResponse   (..)
       , Detection           (..)
       , LanguageResponse    (..)
       , LanguageName        (..)
       , Language            (..)
       ) where
------------------------------------------------------------------------------
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics
import           Servant.API
import           Servant.Client
------------------------------------------------------------------------------
-- | API Key
newtype Key = Key Text
  deriving (ToText, FromText, Show, Eq, Ord)
------------------------------------------------------------------------------
-- | Source Language
newtype Source = Source Lang
  deriving (ToText, FromText, Show, Eq, Ord)
------------------------------------------------------------------------------
-- | Target Language
newtype Target = Target Lang
  deriving (ToText, FromText, Show, Eq, Ord)
------------------------------------------------------------------------------
-- | Text for translation
newtype Body = Body Text
  deriving (ToText, FromText, Show, Eq, Ord)
------------------------------------------------------------------------------
-- | Translated Text
newtype TranslatedText = TranslatedText Text
  deriving (ToText, FromText, Show, Eq, Ord, FromJSON)
------------------------------------------------------------------------------
-- | Translation Reponse
data TranslationResponse = TranslationResponse {
    translations :: [ Translation ]
  } deriving (Show, Eq, Ord, Generic)
------------------------------------------------------------------------------
instance FromJSON TranslationResponse where
  parseJSON (Object o) = do
    d <- o .: "data"
    TranslationResponse <$> d .: "translations"
------------------------------------------------------------------------------
-- | Translation 
data Translation = Translation {
    translatedText :: TranslatedText
  , detectedSourceLanguage :: Maybe Lang
  } deriving (Show, Eq, Generic, Ord)
------------------------------------------------------------------------------
instance FromJSON Translation
------------------------------------------------------------------------------
-- | Deletion Response
data DetectionResponse = DetectionResponse {
     detections :: [ [Detection] ]
  } deriving (Show, Eq, Ord, Generic)
------------------------------------------------------------------------------
instance FromJSON DetectionResponse where
  parseJSON (Object o) = do
    d <- o .: "data"
    DetectionResponse <$> d .: "detections"
------------------------------------------------------------------------------
-- | Detection
data Detection = Detection {
    language   :: Lang
  , isReliable :: IsReliable
  , confidence :: Confidence
  } deriving (Show, Eq, Generic, Ord)
------------------------------------------------------------------------------
-- | Confidence
newtype Confidence = Confidence Double deriving (Show, Eq, Ord, FromJSON)
------------------------------------------------------------------------------
-- | IsReliable
newtype IsReliable = IsReliable Bool deriving (Show, Eq, Ord, FromJSON)
------------------------------------------------------------------------------
instance FromJSON Detection
------------------------------------------------------------------------------
-- | Language Response
data LanguageResponse = LanguageResponse {
     languages :: [Language]
  } deriving (Show, Eq, Ord, Generic)
------------------------------------------------------------------------------
instance FromJSON LanguageResponse where
  parseJSON (Object o) = do
    d <- o .: "data"
    LanguageResponse <$> d .: "languages"
------------------------------------------------------------------------------
-- | Language 
data Language = Language {
     lang :: Text
   , name :: Maybe LanguageName
  } deriving (Show, Eq, Generic, Ord)
------------------------------------------------------------------------------
instance FromJSON Language where
  parseJSON (Object o) = Language <$> o .: "language" <*> o .:? "name"
------------------------------------------------------------------------------
-- | Language Name
newtype LanguageName = LanguageName Text deriving (Show, Eq, Ord, FromJSON)
------------------------------------------------------------------------------
-- | Google Translate API
type API = "language"
        :> "translate"
        :> "v2"
        :> QueryParam "key" Key
        :> QueryParam "source" Source
        :> QueryParam "target" Target
        :> QueryParam "q" Body
        :> Get '[JSON] TranslationResponse
        :<|>
           "language"
        :> "translate"
        :> "v2"
        :> "detect"
        :> QueryParam "key" Key
        :> QueryParam "q" Body
        :> Get '[JSON] DetectionResponse
        :<|>
           "language"
        :> "translate"
        :> "v2"
        :> "languages"
        :> QueryParam "key" Key
        :> QueryParam "target" Target
        :> Get '[JSON] LanguageResponse
------------------------------------------------------------------------------
api :: Proxy API
api = Proxy
------------------------------------------------------------------------------
translate'
  :: Maybe Key
  -> Maybe Source
  -> Maybe Target
  -> Maybe Body
  -> EitherT ServantError IO TranslationResponse
detect'
  :: Maybe Key
  -> Maybe Body
  -> EitherT ServantError IO DetectionResponse
translate' :<|> detect' :<|> getLanguages' = client api (BaseUrl Https "www.googleapis.com" 443)
------------------------------------------------------------------------------
-- | Detect target language
detect
  :: Key
  -> Body
  -> IO (Either ServantError DetectionResponse)
detect key body = runEitherT $ detect' (Just key) (Just body)
------------------------------------------------------------------------------
-- | Perform translation from `Source` language to `Target` langauge.
-- If `Source` not specified, attempt detection of `Lang`
translate
  :: Key
  -> Maybe Source
  -> Target
  -> Body
  -> IO (Either ServantError TranslationResponse)
translate key src trgt body =
  runEitherT $ translate' (Just key) src (Just trgt) (Just body)
------------------------------------------------------------------------------
-- | Retrieve all languages
-- If `Target` specified, return langauge name in `Target` langauge.
getLanguages 
  :: Key
  -> Maybe Target
  -> IO (Either ServantError LanguageResponse)
getLanguages key trgt = runEitherT $ getLanguages' (Just key) trgt
------------------------------------------------------------------------------
toTxt :: Show a => a -> Text
toTxt = T.pack . show 
------------------------------------------------------------------------------
instance Show Lang where
  show Afrikaans          = "af"
  show Albanian           = "sq"
  show Arabic             = "ar"
  show Armenian           = "hy"
  show Azerbaijani        = "az"
  show Basque             = "eu"
  show Belarusian         = "be"
  show Bengali            = "bn"
  show Bosnian            = "bs"
  show Bulgarian          = "bg"
  show Catalan            = "ca"
  show Cebuano            = "ceb"
  show Chichewa           = "ny"
  show ChineseSimplified  = "zh"
  show ChineseTraditional = "zh-TW"
  show Croatian           = "hr"
  show Czech              = "cs"
  show Danish             = "da"
  show Dutch              = "nl"
  show English            = "en"
  show Esperanto          = "eo"
  show Estonian           = "et"
  show Filipino           = "tl"
  show Finnish            = "fi"
  show French             = "fr"
  show Galician           = "gl"
  show Georgian           = "ka"
  show German             = "de"
  show Greek              = "el"
  show Gujarati           = "gu"
  show HaitianCreole      = "ht"
  show Hausa              = "ha"
  show Hebrew             = "iw"
  show Hindi              = "hi"
  show Hmong              = "hmn"
  show Hungarian          = "hu"
  show Icelandic          = "is"
  show Igbo               = "ig"
  show Indonesian         = "id"
  show Irish              = "ga"
  show Italian            = "it"
  show Japanese           = "ja"
  show Javanese           = "jw"
  show Kannada            = "kn"
  show Kazakh             = "kk"
  show Khmer              = "km"
  show Korean             = "ko"
  show Lao                = "lo"
  show Latin              = "la"
  show Latvian            = "lv"
  show Lithuanian         = "lt"
  show Macedonian         = "mk"
  show Malagasy           = "mg"
  show Malay              = "ms"
  show Malayalam          = "ml"
  show Maltese            = "mt"
  show Maori              = "mi"
  show Marathi            = "mr"
  show Mongolian          = "mn"
  show MyanmarBurmese     = "my"
  show Nepali             = "ne"
  show Norwegian          = "no"
  show Persian            = "fa"
  show Polish             = "pl"
  show Portuguese         = "pt"
  show Punjabi            = "pa"
  show Romanian           = "ro"
  show Russian            = "ru"
  show Serbian            = "sr"
  show Sesotho            = "st"
  show Sinhala            = "si"
  show Slovak             = "sk"
  show Slovenian          = "sl"
  show Somali             = "so"
  show Spanish            = "es"
  show Sundanese          = "su"
  show Swahili            = "sw"
  show Swedish            = "sv"
  show Tajik              = "tg"
  show Tamil              = "ta"
  show Telugu             = "te"
  show Thai               = "th"
  show Turkish            = "tr"
  show Ukrainian          = "uk"
  show Urdu               = "ur"
  show Uzbek              = "uz"
  show Vietnamese         = "vi"
  show Welsh              = "cy"
  show Yiddish            = "yi"
  show Yoruba             = "yo"
  show Zulu               = "zu"
------------------------------------------------------------------------------
-- | Languages for translation
data Lang =
    Afrikaans
  | Albanian
  | Arabic
  | Armenian
  | Azerbaijani
  | Basque
  | Belarusian
  | Bengali
  | Bosnian
  | Bulgarian
  | Catalan
  | Cebuano
  | Chichewa
  | ChineseSimplified
  | ChineseTraditional
  | Croatian
  | Czech
  | Danish
  | Dutch
  | English
  | Esperanto
  | Estonian
  | Filipino
  | Finnish
  | French
  | Galician
  | Georgian
  | German
  | Greek
  | Gujarati
  | HaitianCreole
  | Hausa
  | Hebrew
  | Hindi
  | Hmong
  | Hungarian
  | Icelandic
  | Igbo
  | Indonesian
  | Irish
  | Italian
  | Japanese
  | Javanese
  | Kannada
  | Kazakh
  | Khmer
  | Korean
  | Lao
  | Latin
  | Latvian
  | Lithuanian
  | Macedonian
  | Malagasy
  | Malay
  | Malayalam
  | Maltese
  | Maori
  | Marathi
  | Mongolian
  | MyanmarBurmese
  | Nepali
  | Norwegian
  | Persian
  | Polish
  | Portuguese
  | Punjabi
  | Romanian
  | Russian
  | Serbian
  | Sesotho
  | Sinhala
  | Slovak
  | Slovenian
  | Somali
  | Spanish
  | Sundanese
  | Swahili
  | Swedish
  | Tajik
  | Tamil
  | Telugu
  | Thai
  | Turkish
  | Ukrainian
  | Urdu
  | Uzbek
  | Vietnamese
  | Welsh
  | Yiddish
  | Yoruba
  | Zulu
  deriving (Eq, Ord)
------------------------------------------------------------------------------
instance ToText Lang where toText = toTxt
------------------------------------------------------------------------------
instance FromText Lang where
  fromText txt =
    case fromJSON (String txt) of
     Success x -> Just x
     _         -> Nothing
------------------------------------------------------------------------------
instance FromJSON Lang where
  parseJSON (String "af") = pure Afrikaans
  parseJSON (String "sq") = pure Albanian
  parseJSON (String "ar") = pure Arabic
  parseJSON (String "hy") = pure Armenian
  parseJSON (String "az") = pure Azerbaijani
  parseJSON (String "eu") = pure Basque
  parseJSON (String "be") = pure Belarusian
  parseJSON (String "bn") = pure Bengali
  parseJSON (String "bs") = pure Bosnian
  parseJSON (String "bg") = pure Bulgarian
  parseJSON (String "ca") = pure Catalan
  parseJSON (String "ceb") = pure Cebuano
  parseJSON (String "ny") = pure Chichewa
  parseJSON (String "zh") = pure ChineseSimplified
  parseJSON (String "zh-TW") = pure ChineseTraditional
  parseJSON (String "hr") = pure Croatian
  parseJSON (String "cs") = pure Czech
  parseJSON (String "da") = pure Danish
  parseJSON (String "nl") = pure Dutch
  parseJSON (String "en") = pure English
  parseJSON (String "eo") = pure Esperanto
  parseJSON (String "et") = pure Estonian
  parseJSON (String "tl") = pure Filipino
  parseJSON (String "fi") = pure Finnish
  parseJSON (String "fr") = pure French
  parseJSON (String "gl") = pure Galician
  parseJSON (String "ka") = pure Georgian
  parseJSON (String "de") = pure German
  parseJSON (String "el") = pure Greek
  parseJSON (String "gu") = pure Gujarati
  parseJSON (String "ht") = pure HaitianCreole
  parseJSON (String "ha") = pure Hausa
  parseJSON (String "iw") = pure Hebrew
  parseJSON (String "hi") = pure Hindi
  parseJSON (String "hmn") = pure Hmong
  parseJSON (String "hu") = pure Hungarian
  parseJSON (String "is") = pure Icelandic
  parseJSON (String "ig") = pure Igbo
  parseJSON (String "id") = pure Indonesian
  parseJSON (String "ga") = pure Irish
  parseJSON (String "it") = pure Italian
  parseJSON (String "ja") = pure Japanese
  parseJSON (String "jw") = pure Javanese
  parseJSON (String "kn") = pure Kannada
  parseJSON (String "kk") = pure Kazakh
  parseJSON (String "km") = pure Khmer
  parseJSON (String "ko") = pure Korean
  parseJSON (String "lo") = pure Lao
  parseJSON (String "la") = pure Latin
  parseJSON (String "lv") = pure Latvian
  parseJSON (String "lt") = pure Lithuanian
  parseJSON (String "mk") = pure Macedonian
  parseJSON (String "mg") = pure Malagasy
  parseJSON (String "ms") = pure Malay
  parseJSON (String "ml") = pure Malayalam
  parseJSON (String "mt") = pure Maltese
  parseJSON (String "mi") = pure Maori
  parseJSON (String "mr") = pure Marathi
  parseJSON (String "mn") = pure Mongolian
  parseJSON (String "my") = pure MyanmarBurmese
  parseJSON (String "ne") = pure Nepali
  parseJSON (String "no") = pure Norwegian
  parseJSON (String "fa") = pure Persian
  parseJSON (String "pl") = pure Polish
  parseJSON (String "pt") = pure Portuguese
  parseJSON (String "pa") = pure Punjabi
  parseJSON (String "ro") = pure Romanian
  parseJSON (String "ru") = pure Russian
  parseJSON (String "sr") = pure Serbian
  parseJSON (String "st") = pure Sesotho
  parseJSON (String "si") = pure Sinhala
  parseJSON (String "sk") = pure Slovak
  parseJSON (String "sl") = pure Slovenian
  parseJSON (String "so") = pure Somali
  parseJSON (String "es") = pure Spanish
  parseJSON (String "su") = pure Sundanese
  parseJSON (String "sw") = pure Swahili
  parseJSON (String "sv") = pure Swedish
  parseJSON (String "tg") = pure Tajik
  parseJSON (String "ta") = pure Tamil
  parseJSON (String "te") = pure Telugu
  parseJSON (String "th") = pure Thai
  parseJSON (String "tr") = pure Turkish
  parseJSON (String "uk") = pure Ukrainian
  parseJSON (String "ur") = pure Urdu
  parseJSON (String "uz") = pure Uzbek
  parseJSON (String "vi") = pure Vietnamese
  parseJSON (String "cy") = pure Welsh
  parseJSON (String "yi") = pure Yiddish
  parseJSON (String "yo") = pure Yoruba
  parseJSON (String "zu") = pure Zulu
------------------------------------------------------------------------------
instance ToJSON Lang where
  toJSON = String . toTxt


