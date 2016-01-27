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
import           System.Process
import           System.IO

data FileData = FileData {
      ifile :: String,
      ofile :: String
    } deriving Show

data Options = Options { 
      fdata :: Maybe FileData,
      inter :: Bool
    } deriving Show

options :: Parser Options
options = Options
          <$> optional ( FileData 
              <$> strOption 
                 (  long "input-file" 
                 <> short 'i' 
                 <> metavar "INPUTFILE" 
                 <> help "Input file")
              <*> strOption 
                  (  long "output-file-prefix" 
                  <> short 'o' 
                  <> metavar "OUTPUTPREFIX" 
                  <> help "Output file name prefix") )
          <*> switch
                  (  long "interactive"
                  <> help "Interactive mode" )

main :: IO ()
main = do
  Options a intr <- execParser opts
  let (infile, outfile) = if intr then ("","") else (\(Just (FileData i o)) -> (i,o)) a
  mgr <- HTTPClient.newManager HTTPClientTLS.tlsManagerSettings
  if intr 
  then interactive mgr
  else do
    content <- TIO.readFile infile
    let contents = T.splitOn "\n\n" content
    let len = length contents
    forM_ (zip contents [(1::Int)..len]) $ \(x, n) -> do
      TIO.putStrLn $ sformat ("The paragraph " % int % "/" % int % " is being processed.") n len
      speech <- createSpeech x mgr
      let newfilename = outfile ++ T.unpack (sformat ((left (length . show $ len) '0' %. int) % ".mp3") n)
      B.writeFile newfilename speech
      TIO.putStrLn $ sformat ("The file " % string % " has been created.") newfilename 
  where opts = info (helper <*> options)
           ( fullDesc
           <> progDesc "Read INPUTFILE and convert each paragraph to OUTPUTPREFIX<number>.mp3"
           <> header "Ivona - program for audiobook from text creation" )
             
interactive :: HTTPClient.Manager -> IO ()
interactive mgr = do
  putStr "> "
  hFlush stdout
  s <- TIO.getLine
  speech <- createSpeech s mgr
  B.writeFile "lastphrase.mp3" speech
  _ <- system $ "mplayer lastphrase.mp3 &> /dev/null"
  interactive mgr
  
