{-# LANGUAGE OverloadedStrings #-}
module Ivona.Types where

import Data.ByteString
import Data.Aeson
import Data.Monoid
import qualified Data.Text as T
import qualified Control.Applicative as A

data SpeechOptions = SpeechOptions {
      input :: Input,
      outputFormat :: OutputFormat,
      parameters :: Parameters,
      voice :: Voice
} deriving Show

instance FromJSON SpeechOptions where 
    parseJSON (Object v) = SpeechOptions <$> 
                           v .: "Input" <*>
                           v .: "OutputFormat" <*>
                           v .: "Parameters" <*>
                           v .: "Voice"

instance ToJSON SpeechOptions where 
    toJSON (SpeechOptions i o p v) = 
        object ["Input" .= i, "OutputFormat" .= o, "Parameters" .= p, "Voice" .= v]
    toEncoding (SpeechOptions i o p v) = 
        pairs ("Input" .= i <> "OutputFormat" .= o <> "Parameters" .= p <> "Voice" .= v)

data Input = Input {
      ddata :: String,
      ttype :: String
} deriving Show

instance FromJSON Input where
    parseJSON (Object v) = Input <$>
                           v .: "Data" <*>
                           v .: "Type"
    parseJSON _ = A.empty

instance ToJSON Input where 
    toJSON (Input d t) = 
        object ["Data" .= T.pack d, "Type" .= T.pack t]
    toEncoding (Input d t) = 
        pairs ("Data" .= T.pack d <> "Type" .= T.pack t)

data OutputFormat = OutputFormat {
      codec :: String,
      sampleRate :: Int
} deriving Show

instance FromJSON OutputFormat where
    parseJSON (Object v) = OutputFormat <$>
                           v .: "Codec" <*>
                           v .: "SampleRate"
    parseJSON _ = A.empty

instance ToJSON OutputFormat where 
    toJSON (OutputFormat c s) = 
        object ["Codec" .= T.pack c, "SampleRate" .= s]
    toEncoding (OutputFormat c s) = 
        pairs ("Codec" .= T.pack c <> "SampleRate" .= s)

data Parameters = Parameters {
      rate :: String,
      volume :: String,
      sentenceBreak :: Int,
      paragraphBreak :: Int
} deriving Show

instance FromJSON Parameters where
    parseJSON (Object v) = Parameters <$>
                           v .: "Rate" <*>
                           v .: "Volume" <*>
                           v .: "SentenceBreak" <*>
                           v .: "ParagraphBreak"
    parseJSON _ = A.empty

instance ToJSON Parameters where
    toJSON (Parameters r v s p) = 
        object ["Rate" .= T.pack r, "Volume" .= T.pack v, "SentenceBreak" .= s, "ParagraphBreak" .= p]
    toEncoding (Parameters r v s p) = 
        pairs ("Rate" .= T.pack r <> "Volume" .= T.pack v <> "SentenceBreak" .= s <> "ParagraphBreak" .= p)

data Voice = Voice {
      name :: String,
      language :: String,
      gender :: String
} deriving Show

instance FromJSON Voice where
    parseJSON (Object v) = Voice <$>
                           v .: "Name" <*>
                           v .: "Language" <*>
                           v .: "Gender" 
    parseJSON _ = A.empty

instance ToJSON Voice where
    toJSON (Voice n l g) =
        object ["Name" .= T.pack n, "Language" .= T.pack l, "Gender" .= T.pack g]
    toEncoding (Voice n l g) = 
        pairs ("Name" .= T.pack n <> "Language" .= T.pack l <> "Gender" .= T.pack g)

newSpeechOptions :: String -> SpeechOptions
newSpeechOptions d = SpeechOptions { 
                       input = Input { ddata = d, ttype = "text/plain"},
                       outputFormat = OutputFormat { codec = "MP3", sampleRate = 22050 },
                       parameters = Parameters { rate = "medium", volume = "medium", sentenceBreak = 400, paragraphBreak = 640 },
                       voice = Voice { name = "Salli", language = "en-US", gender = "Female" }
}                          
                     