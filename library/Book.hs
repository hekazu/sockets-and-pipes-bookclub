module Book where

-- Provided at start
import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))
-- Chapter 1: Handles
import qualified System.IO as IO
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)

-- Chapter 1: Handles

-- Gets a data directory for us to use, creating it if it does not exist
getDataDir :: IO FilePath
getDataDir = do
  dir <- Dir.getXdgDirectory Dir.XdgData "sockets-and-pipes"
  Dir.createDirectoryIfMissing True dir
  return dir

-- Writes a simple greeting file into the data directory from `getDataDir`
writeGreetingFile :: IO ()
writeGreetingFile = do
  dir <- getDataDir
  h <- IO.openFile (dir </> "greeting.txt") WriteMode
  IO.hPutStrLn h "hello"
  IO.hPutStrLn h "world"
  IO.hClose h

-- A more error safe version of above, demonstrating trying and some default
-- handle behaviour via writing to stderr
writeGreetingTry :: IO ()
writeGreetingTry = do
  dir <- getDataDir
  IO.hPutStrLn IO.stderr "About to open the file :/"
  openResult <- tryAny $ IO.openFile (dir </> "greeting.txt") WriteMode
  case openResult of
    Left _ -> IO.hPutStrLn IO.stderr "Cannot open file to write :("
    Right h -> do
      IO.hPutStrLn h "hello"
      IO.hPutStrLn h "world"
      IO.hClose h
      IO.hPutStrLn IO.stderr "Done :)"

-- Now with additional safety via the ResourceT monad transformer (hence the T)
writeGreetingSafe :: IO ()
writeGreetingSafe = runResourceT @IO do
  dir <- liftIO getDataDir
  (_releaseKey, h) <-
    allocate (IO.openFile (dir </> "greeting.txt") WriteMode) IO.hClose
  liftIO $ IO.hPutStrLn h "hello"
  liftIO $ IO.hPutStrLn h "world"
