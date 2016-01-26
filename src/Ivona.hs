{-# LANGUAGE OverloadedStrings #-}
module Ivona where

import Data.Aeson
import Control.Lens
import Data.Text (Text)
import Data.ByteString.Lazy
import Ivona.Types
import qualified Network.HTTP.Client as HTTPClient
import Network.Wreq

ivonaAPI :: String
ivonaAPI = "https://tts.eu-west-1.ivonacloud.com"

createSpeechAPI :: String
createSpeechAPI = ivonaAPI ++ "/CreateSpeech"

listVoicesAPI :: String
listVoicesAPI = ivonaAPI ++ "/ListVoices"

createSpeech :: Text -> HTTPClient.Manager -> IO ByteString 
createSpeech s mgr = do
  let opts = defaults & manager .~ (Right mgr)
                      & auth ?~ awsAuth AWSv4 "GDNAI7ROPRZLGYVRAJYQ" "x4yaAVE5dWl7+sU2kkxAgpGsd68/8kdDYHNekoHj" 
                      & header "Content-Type" .~ ["application/json"]
  r <- postWith opts createSpeechAPI (toJSON $ newSpeechOptions s)
  return (r ^. responseBody)
 
