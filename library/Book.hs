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
-- @IO is not needed since we have the signature, but I didn't want GHC complaining
-- at at every turn. If we omitted the signature though, it would be used to infer
-- the base monad here.
writeGreetingSafe = runResourceT @IO do
  dir <- liftIO getDataDir
  (_releaseKey, h) <-
    fileResource (dir </> "greeting.txt") WriteMode
  liftIO $ IO.hPutStrLn h "hello"
  liftIO $ IO.hPutStrLn h "world"

-- Exercises!
--
-- 1. File resource function
-- Define a function with the following type signature:
fileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
fileResource path mode = allocate (IO.openFile path mode) IO.hClose
-- and then rewrite writeGreetingSafe to use it!
--
-- Note the creation with `allocate`. The important parts are:
--   - The first function, of type IO a, opening a file and delivering the handle
--   - The second function, of type (a -> IO ()), closing the file handle

-- 2. Showing handles
-- Handle *does* have a Show instance, but it's somewhat puny. Luckily we have
-- IO.hShow just as well. Let's write a handy comparison function to remind us of
-- the differences:
handlePrintTest :: IO ()
handlePrintTest = runResourceT do
  dir <- liftIO getDataDir
  -- We elect to use the greeting.txt in read mode for a bit of a change, but
  -- nothing too extreme to maintain incremental change for easier debugging
  (_releaseKey, handle) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO $ putStrLn "Show instance for Handle:"
  liftIO $ putStrLn $ show handle
  liftIO $ putStrLn "Proper Handle showing:"
  detailedHandleInfo <- liftIO $ IO.hShow handle
  liftIO $ putStrLn detailedHandleInfo
  -- That sure is some heavy lifting, huh?
  -- All in the name of safety, one supposes!

-- 3. Exhaustion
-- So when do we run out of handles again? Let's give that a whirl...
--
-- This one we had as a freebie
howManyHandles :: IO ()
howManyHandles = runResourceT do
  hs <- openManyHandles
  putStrLn ("Opened " <> show (length hs) <> " handles")

-- This we needed to build ourselves, with the goal to open handles until
-- the OS tells us no
openManyHandles :: ResourceT IO [Handle]
openManyHandles = do
  maybeHandle <- fileResourceMaybe
  case maybeHandle of
    Nothing -> return []
    Just h ->
      fmap (h:) openManyHandles

-- And this is where we, perhaps, open a handle. We had to fill in the tryAny.
-- We also had to fix the return on a succesful handle getting, but that's of
-- little consequence and not the actual point of the exercise.
fileResourceMaybe :: ResourceT IO (Maybe Handle)
fileResourceMaybe = do
  dir <- liftIO getDataDir
  result <- tryAny do
    -- Note that WriteMode already fails at 1, since y'know, it reserves the file
    -- for writing. ReadMode gets many more.
    liftIO $ IO.openFile (dir </> "overloadHandleTest.txt") ReadMode
  case result of
    Right h -> return (Just h)
    Left e -> do
      print (displayException e)
      return Nothing
