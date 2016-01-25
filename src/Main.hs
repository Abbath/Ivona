{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as B
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Ivona
import           System.Environment
import           Text.Printf

main :: IO ()
main = do
  [infile, outfile] <- getArgs
  content <- TIO.readFile infile
  let contents = T.splitOn "\n\n" content
  mapSpeech contents outfile 1 

mapSpeech :: [Text] -> String -> Int -> IO ()
mapSpeech [] _ _ = return ()
mapSpeech (x:xs) outfile n = do
  speech <- createSpeech x
  B.writeFile (outfile ++ printf "%04d" n ++ ".mp3") speech
  mapSpeech xs outfile (n+1)
             
  
