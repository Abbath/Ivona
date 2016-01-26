{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as B
import           Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Formatting
import           Ivona
import           Options.Applicative
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.TLS as HTTPClientTLS

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
  mgr <- HTTPClient.newManager HTTPClientTLS.tlsManagerSettings
  forM_ (zip contents [(1::Int)..]) $ \(x, n) -> do
    TIO.putStrLn $ sformat ("Paragraph number " % int % " is being processed.") n 
    speech <- createSpeech x mgr
    let newfilename = outfile ++ T.unpack (sformat ((left 4 '0' %. int) % ".mp3") n)
    B.writeFile newfilename speech
    TIO.putStrLn $ sformat ("File " % string % " has been created.") newfilename 
  where opts = info (helper <*> options)
           ( fullDesc
           <> progDesc "Read INPUTFILE and convert each paragraph to OUTPUTFILE<number>.mp3"
           <> header "Ivona - program for audiobook from text creation" )
             
  
