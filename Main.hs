{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE DeriveGeneric #-}

--import Data.Aeson (toJSON)
--import Data.Csv (encode)
import Data.Aeson 
import Data.Text (Text)
import GHC.Generics

import Network.URI (
  parseURI,
  parseAbsoluteURI,
  parseURIReference,
  uriPath,
  uriAuthority,
  uriPort,
  uriRegName,
  uriScheme,
  URI)

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Internal (ByteString)
import Control.Monad.Catch (MonadThrow)
import Network.HTTP.Simple (
  parseRequest,
  getResponseStatus,
  getResponseStatusCode,
  setRequestMethod,
  --setRequestResponseTimeout,
  ResponseHeaders,
  Request,
  Response,
  httpBS)

--import Network.HTTP.Base (mkRequest)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Options.Applicative

import System.IO (appendFile)


data OptionFlags = OptionFlags
  { threads  :: Integer
  , verbose  :: Bool
  , dlist    :: FilePath
  , domain   :: String
  } deriving (Show)
  
data ResponseEntry = ResponseEntry
  { 
    --rsHeader :: ResponseHeaders
    rsUrl :: String
  , rsStatusCode :: Int
  }
 
instance ToJSON ResponseEntry where
  toJSON (ResponseEntry url scode) = object ["url" .= url, "statuscode" .= scode] 

--instance ToJSON OptionFlags where  
--  toJSON'     = genericToJSON customOptions
--  toEncoding' = genericToEncoding customOptions

 --b = encode [("John" :: Text, 27 :: Int), ("Jane", 28)]
data Verbose = V | VV | VVV


optionsParser :: Parser OptionFlags
optionsParser = let 
  
    threadsParser :: Parser Integer
    threadsParser = option auto (
      long "threads" <> 
      short 't' <>
      help "number of threads" <>
      value 1 <>
      metavar "NUMBER")
    
    verboseParser :: Parser Bool
    verboseParser = vParser <|> vvParser <|> vvvParser
      where
      vParser = switch (long "verbose" <> short 'v')
      vvParser = switch (long "vv")
      vvvParser = switch (long "vvv")
   
    dlistParser :: Parser FilePath
    dlistParser = strOption (long "dlist" <> short 'l' <> help "directory list" <> metavar "FILE")  
    
    targetIpOrDomainParser :: Parser String
    targetIpOrDomainParser = strArgument (metavar "IP/DOMAIN") 
    
  in 
    OptionFlags <$> threadsParser <*> verboseParser <*> dlistParser <*> targetIpOrDomainParser


extractWordList :: FilePath -> IO ([String])
extractWordList fp = do
  content <- readFile fp
  return $ lines content


--makeRequest :: String -> IO()
--makeRequest uri = let 
--  h = [mkHeader HdrAllow "*"]
--  r = case parseURI uri of 
--        Just x -> mkRequest GET x
--        Nothing -> error $ uri ++ " is not a valid URI!"
--  in do
--    res <- Network.HTTP.simpleHTTP r
--    getResponseCode res >>= print

extractURI :: String -> String
extractURI url = let 
  uri = case parseURI url of
    Just x -> x
    Nothing -> error $ url ++ " is not a valid uri!"
   -- handle Maybe Regname with custom error
  uriauth = case uriAuthority uri of
    Just x -> x
    Nothing -> error $ "Missing or corrupt uri authority!"
  proto = uriScheme uri
  port = uriPort uriauth
  --uripath = uriPath uri
  regName = uriRegName uriauth
  in 
    proto ++ "//" ++ regName ++ port
  
--makeRequest :: String -> IO(Response ByteString)
--makeRequest url = let
--   uri = case parseURI url of
--    Just x -> x
--    Nothing -> error $ url ++ " is not a valid URI!"
--   -- handle Maybe Regname with custom error
--   regName = fromJust $ uriRegName <$> uriAuthority uri
--  in do 
--    req <- parseRequest url
--    httpBS req 

--parseResponse :: Response -> String
--parseResponse res = let 
--    statuscode = getResponseStatus res
--  in
--    encode $ ResponseEntry "" statuscode
extractJSON :: String -> Response a -> String
extractJSON url r = unpack $ encode re
  where 
    re = ResponseEntry{
      rsUrl=url, 
      rsStatusCode=getResponseStatusCode r
    }


log2File :: FilePath -> String -> IO()
log2File fp line = appendFile fp line

mainLoop :: OptionFlags -> String -> IO()
mainLoop opt url = do
  res <- httpBS =<< (parseRequest url)
  let json = extractJSON url res
    in do 
      log2File "./test" $ json ++ "\n"
      print json


main :: IO()
main = do
  opt <- execParser opts
  print opt
  putStrLn "Parsing uri path from dir list!"
  uripaths <- extractWordList $ dlist opt 
  putStrLn "Validate provided domain!"
  baseurl <- return $ extractURI $ domain opt
  urls <- let 
      urlBuilder = (++) (baseurl ++ "/")
    in 
      return $ map urlBuilder uripaths 
  mapM_ (mainLoop opt) urls
  where
--    printStatusCode :: String -> Int -> IO()
--    printStatusCode url code = putStrLn $ url ++ ": " ++ show(code)
--    
--    makeRequest :: String -> IO()
--    makeRequest url = do
--      req <- parseRequest url
--      --limReq <- return $ req { Request.responseTimeout = 5 }
--      code <- getResponseStatusCode <$> httpBS req 
--      printStatusCode url code 
      
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "=== DirBuster Haskell Version ==="
     <> header "DirBuster" )
