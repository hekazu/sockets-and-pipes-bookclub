module Book where

-- Provided at start
import Relude
import qualified System.Directory as Dir
import System.FilePath ((</>))
-- Chapter 1: Handles
import qualified System.IO as IO
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, allocate, runResourceT)
--Chapter 2: Chunks
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Char as Char
-- Chaper 3: Bytes
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

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


-- Chapter 2: Chunks

-- Quick rewrite of Hello World examples using Text
helloText :: IO ()
helloText = T.hPutStrLn stdout $ T.pack "hello world!"

helloTextFile :: IO ()
helloTextFile = runResourceT do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") WriteMode
  liftIO do
    T.hPutStrLn h $ T.pack "hello"
    T.hPutStrLn h $ T.pack "world"

-- Reading from a handle
-- We do not read lines because someone might give us infinite data. Let's do
-- chunks instead!
printFileContentsUpperCase :: IO ()
printFileContentsUpperCase = runResourceT do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO $ printCapitalizedText h

printCapitalizedText :: Handle -> IO ()
printCapitalizedText h = proceed
  where
    proceed = do
      -- Get a chunk
      chunk <- T.hGetChunk h
      -- If the chunk is empty, we are done, otherwise we process the chunk
      case (T.null chunk) of
        True -> return ()
        -- Changed inline `do` to `(>>)` implementation
        False -> T.putStr (T.toUpper chunk) >> proceed

-- Writing that every time would be a bit of a chore. Let's make an abstraction:
repeatUntilIO :: IO chunk -> (chunk -> Bool) -> (chunk -> IO ()) -> IO ()
repeatUntilIO getChunk isEnd f = proceed
  where
    proceed = do
      chunk <- getChunk
      case (isEnd chunk) of
        True -> return ()
        False -> f chunk >> proceed

-- And now the above, using our abstraction:
printFileContentsUpperCase2 :: IO ()
printFileContentsUpperCase2 = runResourceT @IO do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> "greeting.txt") ReadMode
  liftIO $ repeatUntilIO (T.hGetChunk h) T.null \chunk -> T.putStr $ T.toUpper chunk

-- Exercises!
--
-- 4. Find the Numbers
-- Get only numeric characters from a Text
digitsOnly :: Text -> Text
digitsOnly = T.filter Char.isDigit

-- 5. Capitalize the last
-- Capitalize the last letter of a Text
capitalizeLast :: Text -> Text
capitalizeLast text = case T.unsnoc text of
  Just (front, final) -> T.snoc front $ Char.toUpper final
  Nothing -> T.empty

-- 6. Paren removal
-- Remove one layer of parentheses from the text if possible
unParen :: Text -> Maybe Text
unParen text = case T.uncons text of
  Just ('(',rest) -> case T.unsnoc rest of
    Just (stripped,')') -> Just stripped
    _ -> Nothing
  _ -> Nothing

-- 7. Character count
-- Count the characters in a file, by refactoring an old solution into
-- something nicer
characterCount :: FilePath -> IO Int
characterCount fp = runResourceT do
  dir <- liftIO getDataDir
  (_, h) <- fileResource (dir </> fp) ReadMode
  liftIO $ countChunkChars h

countChunkChars :: Handle -> IO Int
countChunkChars h = proceed 0
  where
    proceed x = do
      chunk <- T.hGetChunk h
      if T.null chunk then return x else proceed $ x + T.length chunk

-- 8. Beyond IO
-- Since `repeatUntilIO` doesn't actually do IO specifics, let's generalise it
repeatUntil :: Monad m => m chunk -> (chunk -> Bool) -> (chunk -> m ()) -> m ()
repeatUntil getChunk isEnd f = proceed
  where
    proceed = do
      chunk <- getChunk
      unless (isEnd chunk) $ f chunk >> proceed

-- 9. When and unless
-- We modify the above function to use either `when` or `unless` from
-- `Control.Monad`


-- Chapter 3: Bytes

exampleBytes :: [Word8]
exampleBytes = [104, 101, 108, 108, 111]

-- We have written, we have read, now let us do both!
copyGreetingFile :: IO ()
copyGreetingFile = runResourceT do
  dir <- liftIO getDataDir
  (_,h1) <- binaryFileResource (dir </> "greeting.txt") ReadMode
  (_,h2) <- binaryFileResource (dir </> "greeting2.txt") WriteMode
  -- We avoided defining a lambda here in the action to do with the chunks
  -- given HLS is rather unhappy about some decisions made in representation.
  --
  -- This may not be optimal for the material itself (less digestible as there
  -- are tricks involved), but it should reinforce the learning to see how things
  -- actually work under the hood rather than copy-pasting.
  liftIO $ repeatUntil (BS.hGetSome h1 1_024) BS.null (BS.hPutStr h2)

binaryFileResource :: FilePath -> IOMode -> ResourceT IO (ReleaseKey, Handle)
binaryFileResource path mode = allocate (IO.openBinaryFile path mode) IO.hClose

-- And now, what you have all been waiting for, a Hello World that produces
-- specifically UTF-8 encoded and Unix line terminated output!
helloByteString :: IO ()
helloByteString = do
  IO.hSetBinaryMode stdout True
  BS.hPut stdout $ BS.pack helloBytes

helloBytes :: [Word8]
helloBytes = [
  104, 101, 108, 108, 111,      -- hello
  32,                           -- space
  119, 111, 114, 108, 100, 33,  -- world!
  10 ]                          -- \n

helloUtf8 :: IO ()
helloUtf8 = do
  IO.hSetBinaryMode stdout True
  BS.hPutStr stdout . T.encodeUtf8 $ T.pack "hello world!\n"

-- Exercises!
--
-- 10. Character encoding bug
-- How may we misuse the below function to produce "wrong" output?
greet :: ByteString -> IO ()
greet nameBs = case T.decodeUtf8' nameBs of
  Left _ -> putStrLn "Invalid byte string"
  Right nameText -> T.putStrLn $ T.pack "Hello, " <> nameText
-- Answer: Feed it UTF-32 encoded data with umlauts, e.g.
--   greet . T.encodeUtf32LE $ T.pack "Älli Pöllönen"

-- 11. Byte manipulation
-- Make the following function convert lowercase ASCII characters to uppercase
asciiUpper :: ByteString -> ByteString
asciiUpper = BS.map \w -> if w > 96 then w - 32 else w
