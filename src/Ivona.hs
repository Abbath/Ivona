{-# LANGUAGE OverloadedStrings #-}
module Ivona where

import Data.Aeson
import Control.Lens
import Data.ByteString.Lazy
import Ivona.Types
import Network.Wreq
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout)

ivonaAPI :: String
ivonaAPI = "https://tts.eu-west-1.ivonacloud.com"

createSpeechAPI :: String
createSpeechAPI = ivonaAPI ++ "/CreateSpeech"

listVoicesAPI :: String
listVoicesAPI = ivonaAPI ++ "/ListVoices"

createSpeech :: String -> IO ByteString 
createSpeech s = do
  let opts = defaults & auth ?~ awsAuth AWSv4 "GDNAI7ROPRZLGYVRAJYQ" "x4yaAVE5dWl7+sU2kkxAgpGsd68/8kdDYHNekoHj" 
                      & header "Content-Type" .~ ["application/json"]
  r <- postWith opts createSpeechAPI (toJSON $ newSpeechOptions s)
  return (r ^. responseBody)
 