{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as B
import           Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Formatting
import           Ivona
import           Options.Applicative

data Options = Options { 
      ifile :: String,
      ofile :: String
      }

options :: Parser Options
options = Options
          <$> strOption 
                 (  long "input-file" 
                 <> short 'i' 
                 <> metavar "INPUTFILE" 
                 <> help "Input file")
          <*> strOption 
                  (  long "output-file" 
                  <> short 'o' 
                  <> metavar "OUTPUTFILE" 
                  <> help "Output file")

main :: IO ()
main = do
  Options infile outfile <- execParser opts
  content <- TIO.readFile infile
  let contents = T.splitOn "\n\n" content
  forM_ (zip contents [(1::Int)..]) $ \(x, n) -> do
         speech <- createSpeech x
         B.writeFile (outfile ++ T.unpack (sformat (left 4 '0' %. int) n)) speech
  where opts = info (helper <*> options)
               ( fullDesc
                 <> progDesc "Read INPUTFILE and convert each paragraph to OUTPUTFILE<number>.mp3"
                 <> header "Ivona - program for audiobook from text creation" )
             
  
