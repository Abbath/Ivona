module Main where

import qualified Data.ByteString.Lazy as B
import           Data.List.Split
import           Ivona
import           System.Environment
import           Text.Printf

main :: IO ()
main = do
  [infile, outfile] <- getArgs
  content <- Prelude.readFile infile
  let contents = splitOn "\n\n" content
  mapSpeech contents outfile 1 

mapSpeech :: [String] -> String -> Int -> IO ()
mapSpeech [] _ _ = return ()
mapSpeech (x:xs) outfile n = do
  speech <- createSpeech x
  B.writeFile (outfile ++ printf "%04d" n ++ ".mp3") speech
  mapSpeech xs outfile (n+1)
             
  
