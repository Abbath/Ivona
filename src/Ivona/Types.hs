{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Ivona.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics

data SpeechOptions = SpeechOptions {
      soptInput :: Input,
      soptOutputFormat :: OutputFormat,
      soptParameters :: Parameters,
      soptVoice :: Voice
} deriving (Generic, Show)

instance FromJSON SpeechOptions where

instance ToJSON SpeechOptions where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 4 }

data Input = Input {
      inptData :: Text,
      inptType :: Text
} deriving (Generic, Show)

instance FromJSON Input where

instance ToJSON Input where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 4 }

data OutputFormat = OutputFormat {
      outpCodec :: Text,
      outpSampleRate :: Int
} deriving (Generic, Show)

instance FromJSON OutputFormat where

instance ToJSON OutputFormat where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 4 }

data Parameters = Parameters {
      paramRate :: Text,
      paramVolume :: Text,
      paramSentenceBreak :: Int,
      paramParagraphBreak :: Int
} deriving (Generic, Show)

instance FromJSON Parameters where

instance ToJSON Parameters where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5 }

data Voice = Voice {
      voiceName :: Text,
      voiceLanguage :: Text,
      voiceGender :: Text
} deriving (Generic, Show)

instance FromJSON Voice where

instance ToJSON Voice where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 5}

newSpeechOptions :: Text -> SpeechOptions
newSpeechOptions d = SpeechOptions {
                       soptInput = Input { inptData = d, inptType = "text/plain"},
                       soptOutputFormat = OutputFormat { outpCodec = "MP3", outpSampleRate = 22050 },
                       soptParameters = Parameters { paramRate = "medium", paramVolume = "medium", paramSentenceBreak = 400, paramParagraphBreak = 640 },
                       soptVoice = Voice { voiceName = "Salli", voiceLanguage = "en-US", voiceGender = "Female" }
}
