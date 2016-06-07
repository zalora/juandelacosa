{-# LANGUAGE QuasiQuotes #-}

module Main (
  main
) where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromJust)
import Data.Version (showVersion)
import Database.MySQL.Base (ConnectInfo(..))
import Database.MySQL.Base.Types (Option(ReadDefaultFile, ReadDefaultGroup))
import Paths_juandelacosa (getDataDir, version) -- from cabal
import System.Environment (getArgs)
import Text.InterpolatedString.Perl6 (qc)
import qualified System.Console.Docopt.NoTH as O

import Server (server)

usage :: IO String
usage = do
  dataDir <- getDataDir
  return $
    "juandelacosa " ++ showVersion version
    ++ " manage MariaDB user and roles" ++ [qc|

Usage:
  juandelacosa [options]

Options:
  -f, --file=MYCNF         Read this MySQL client config file
  -g, --group=GROUP        Read this options group in the above file [default: client]

  -d, --datadir=DIR        Data directory including static files [default: {dataDir}]

  -s, --socket=SOCK        Listen on this UNIX-socket [default: /tmp/juandelacosa.sock]
  -p, --port=PORT          Instead of UNIX-socket, listen on this TCP port (localhost)

  -h, --help               Show this message

|]

main :: IO()
main = do
  doco <- O.parseUsageOrExit =<< usage
  args <- O.parseArgsOrExit doco =<< getArgs
  if args `O.isPresent` O.longOption "help"
  then putStrLn $ O.usage doco
  else do
    let
      file = O.getArg args $ O.longOption "file"
      group = fromJust $ O.getArg args $ O.longOption "group"
      port = O.getArg args $ O.longOption "port"
      socket = fromJust $ O.getArg args $ O.longOption "socket"
      datadir = fromJust $ O.getArg args $ O.longOption "datadir"
    -- XXX: mysql package maps empty strings to NULL
    -- which is what we need, see documentation for mysql_real_connect()
    let myInfo = ConnectInfo {
        connectDatabase = "",
        connectHost     = "",
        connectOptions  = case file of
                          Nothing -> []
                          Just f -> [ ReadDefaultFile f, ReadDefaultGroup (pack group) ],
        connectPassword = "",
        connectPath     = "",
        connectPort     = 0,
        connectSSL      = Nothing,
        connectUser     = ""
      }
    let listen = case port of
          Nothing -> Right socket
          Just p -> Left $ read p
    server listen myInfo datadir

