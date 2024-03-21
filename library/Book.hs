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
-- Chapter 4: Sockets
import Network.Socket()
import Network.Socket as S
import Network.Socket.ByteString as S
-- Chapter 5: HTTP
import qualified ASCII as A
import qualified ASCII.Char as A
import Network.Simple.TCP (serve, HostPreference (..))
import qualified Network.Simple.TCP as Net
-- Chapter 6: HTTP Types
import ASCII (ASCII)
import ASCII.Decimal (Digit(..))
import qualified Data.ByteString.Lazy as LBS
-- Chapter 7: Encoding
import qualified Data.ByteString.Builder as BSB
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Time as Time
-- Chapter 9: Content Types
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Encoding as LT
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as HTML
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as J.Key
import qualified Data.Aeson.KeyMap as J.KeyMap
import Data.Aeson (ToJSON (toJSON), (.=))
-- Chapter 10: Change
import qualified Control.Concurrent.Async as Async
-- Chapter 11: Streaming
import Control.Concurrent (threadDelay)
-- Chapter 12: Pipes
import Pipes (Producer, Consumer, Pipe, (>->), yield, await, runEffect)
-- Chapter 13: Parsing
import qualified Data.Map.Strict as Map
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Run as P
import Data.Attoparsec.ByteString (Parser, (<?>))
-- Chapter 14: Errors
import Unfork (unforkAsyncIO_)


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


-- Chapter 4: Sockets

-- Might work, but we forget to close the socket (whoops)
makeFriend :: S.SockAddr -> IO ()
makeFriend address = do
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.connect s address
  S.sendAll s . T.encodeUtf8 $ T.pack "Hello, will you be my friend?"
  repeatUntil (S.recv s 1_024) BS.null BS.putStr

-- Okay, now we should be good for safety, right? But what if we wanted to use a
-- non-stream socket and/or IPv6?
makeFriendSafely :: S.SockAddr -> IO ()
makeFriendSafely address = runResourceT do
  (_, s) <- allocate (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close
  liftIO do
    S.setSocketOption s S.UserTimeout 1_000
    S.connect s address
    S.sendAll s . T.encodeUtf8 $ T.pack "Hello, will you be my friend?"
    repeatUntil (S.recv s 1_024) BS.null BS.putStr
    S.gracefulClose s 1_000

-- Let's just get all the means to connect somewhere, and...
findHaskellWebsite :: IO S.AddrInfo
findHaskellWebsite = do
  addrInfos <- S.getAddrInfo
    (Just S.defaultHints { S.addrSocketType = S.Stream })
    (Just "www.haskell.org")
    (Just "http")
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x

-- Presto! A more general version!
makeFriendAddrInfo :: S.AddrInfo -> IO ()
makeFriendAddrInfo addressInfo = runResourceT do
  (_, s) <- allocate (S.openSocket addressInfo) S.close
  liftIO do
    S.setSocketOption s S.UserTimeout 1_000
    S.connect s $ S.addrAddress addressInfo
    S.sendAll s . T.encodeUtf8 $ T.pack "Hello, will you be my friend?"
    repeatUntil (S.recv s 1_024) BS.null BS.putStr
    S.gracefulClose s 1_000

-- Exercises!
--
-- 12. Improper ResourceT allocation
-- Since we need to open a socket and connect to make any use of a socket,
-- why don't we combine that?
openAndConnect' :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openAndConnect' addressInfo = allocate setup S.close
  where
    setup = do
      s <- S.openSocket addressInfo
      S.setSocketOption s S.UserTimeout 1_000
      S.connect s $ S.addrAddress addressInfo
      return s
-- LGTM! Or does it? What's wrong here?
--   Answer: If connect fails during setup, we are not yet ready to clean up via
--     ResourceT. We should instead perform the connection *after* allocation.
-- Which we now demonstrate here for Chapter 5 exercises.
openAndConnect :: S.AddrInfo -> ResourceT IO (ReleaseKey, Socket)
openAndConnect addressInfo = do
  (rk, s) <- allocate (S.openSocket addressInfo) S.close
  liftIO do
    S.setSocketOption s S.UserTimeout 1_000
    S.connect s $ S.addrAddress addressInfo
  return (rk, s)

-- 13. Explore Gopherspace
-- Let us define a variant of findHaskellWebsite that uses the Gopher protocol
findGopherSite :: IO S.AddrInfo
findGopherSite = do
  addrInfos <- S.getAddrInfo
    Nothing
    (Just "gopher.metafilter.com")
    (Just "gopher")
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x

contactGopherSite :: IO ()
contactGopherSite = runResourceT do
  addrInfo <- liftIO findGopherSite
  (_, s) <- allocate (S.openSocket addrInfo) S.close
  liftIO do
    S.connect s $ S.addrAddress addrInfo
    S.sendAll s . T.encodeUtf8 $ T.pack "\r\n"
    repeatUntil (S.recv s 1_024) BS.null BS.putStr
    S.gracefulClose s 1_000

-- 14. Address resolution
-- Well, since resolving addresses is pretty common let's just generalise it
-- while we are at it, shall we?
resolve :: S.ServiceName -> S.HostName -> IO S.AddrInfo
resolve sName hName = do
  addrInfos <- S.getAddrInfo
    Nothing
    (Just hName)
    (Just sName)
  case addrInfos of
    [] -> fail "getAddrInfo returned []"
    x : _ -> return x


-- Chapter 5: HTTP
--
-- Where we begin reading copious amounts of RFCs

-- First things first, we do take an example supplied by RFC 9110 and turn
-- it into an ASCII representation. Note that we use the Windows format of
-- line break when it comes to HTTP, for so it is specified.
helloRequestString :: ByteString
helloRequestString = fromString $
  "GET /hello.txt HTTP/1.1\r\nUser-Agent: curl/7.64.1\r\n" <>
  "Host: www.example.com\r\nAccept-Language: en, mi\r\n\r\n"

-- Given the above way is tedious, let us help ourselves out some
rawLine :: ByteString -> ByteString
rawLine = (<> fromString "\r\n")

-- Better.
helloRequestLineString :: ByteString
helloRequestLineString =
  rawLine (fromString "GET /hello.txt HTTP/1.1") <>
  rawLine (fromString "User-Agent: curl/7.64.1") <>
  -- host rawLine was initially missing from the example
  rawLine (fromString "Host: www.example.com") <>
  rawLine (fromString "Accept-Language: en, mi") <>
  rawLine (fromString "")

-- We still have the footgun of Unicode though. We ought not to use it here
-- where the spec specifically demands ASCII, and Haskell String literals are
-- Unicode by default. Well, we do also have ASCII support. Observe:
helloRequestASCIIString :: ByteString
helloRequestASCIIString =
  line [A.string|GET /hello.txt HTTP/1.1|] <>
  line [A.string|User-Agent: curl/7.64.1|] <>
  line [A.string|Host: www.example.com|] <>
  line [A.string|Accept-Language: en, mi|] <>
  line [A.string||]

crlf :: [A.Char]
crlf = [A.CarriageReturn, A.LineFeed]

line :: ByteString -> ByteString
line = (<> A.fromCharList crlf)

-- I didn't want to replace any previous definitions. I sure hope hanging on
-- to these and inventing more names for the namespace won't end up biting me.
-- I have, at least, ensured the outputs of all the above helloRequestXXXString
-- functions are equal to one another

-- I don't think this will have much value, but let's include it too since all
-- the requests were included already
helloResponseString :: ByteString
helloResponseString =
  line [A.string|HTTP/1.1 200 OK|] <>
  line [A.string|Content-Type: text/plain; charset=us-ascii|] <>
  line [A.string|Content-Length: 6|] <>
  line [A.string||] <>
  -- Make note of not using `line` here. The body is a Wild West bereft of
  -- rules. Anything goes there, as long as the length (in bytes) checks out.
  [A.string|Hello!|]

-- This marks the spot where we begin to shift away from the network library and
-- moving towards network-simple.
-- Chief functions to consider:
--   S.sendAll -> Net.send (IO vs MonadIO)
--   S.recv -> Net.recv (empty marks end VS Maybe as return type)
--   serve (this exists now)

ourFirstServer :: IO ()
ourFirstServer = serve @IO HostAny "8000" \(s, a) -> do
  putStrLn $ "New connection from " <> show a
  Net.send s helloResponseString


-- Exercises!
--
-- 15. Repeat until nothing
-- We've got `repeatUntil` in exercise 8. It is now time to apply Maybe
-- processing in place of checking for chunk emptiness.
repeatUntilNothing :: Monad m => m (Maybe chunk) -> (chunk -> m ()) -> m ()
repeatUntilNothing getChunk f = proceed
  where
    proceed = whenJustM getChunk f >> proceed

-- Why yes, we *can* define `repeatUntil` via repeatUntilNothing. Not the other
-- way around though, I don't think. I assume to be soon proven wrong.
repeatUntil' :: Monad m => m chunk -> (chunk -> Bool) -> (chunk -> m ()) -> m ()
repeatUntil' getChunk isEnd = repeatUntilNothing getMaybeChunk
  where
    getMaybeChunk = do
      chunk <- getChunk
      return $ if isEnd chunk then Nothing else Just chunk
-- FWIW I was proven wrong, but my solution was the better kind anyway so we
-- don't end up doing tons of extra work for nothing.

-- 16. Make an HTTP request
-- We shall query haskell.org and see what we get.
-- Restrictions: Use `repeatUntilNothing`, `openAndConnect`, `resolve` and
-- `line`
queryHaskellDotOrg :: IO ()
queryHaskellDotOrg = runResourceT do
  addrInfo <- liftIO $ resolve "http" "haskell.org"
  (_, s) <- openAndConnect addrInfo
  Net.send s query
  liftIO $ repeatUntilNothing (Net.recv s 1_024) BS.putStr
  where
    query :: ByteString
    query =
      line [A.string|GET / HTTP/1.1|] <>
      line [A.string|Host: haskell.org|] <>
      line [A.string|Connection: close|] <>
      line [A.string||]


-- Chapter 6: HTTP Types
--
-- Where we take a moment to represent HTTP as Haskell types
data Request = Request RequestLine [Field] (Maybe Body) deriving Show
data Response = Response StatusLine [Field] (Maybe Body) deriving (Eq, Show)

data RequestLine = RequestLine Method RequestTarget Version deriving Show
data Method = Method (ASCII ByteString) deriving (Eq, Show)
data RequestTarget = RequestTarget (ASCII ByteString) deriving Show
data Version = Version Digit Digit deriving (Eq, Show)

data StatusLine = StatusLine Version StatusCode (Maybe ReasonPhrase) deriving (Eq, Show)
data StatusCode = StatusCode Digit Digit Digit deriving (Eq, Show)
data ReasonPhrase = ReasonPhrase (ASCII ByteString) deriving (Eq, Show)

data Field = Field FieldName FieldValue deriving (Eq, Show)
data FieldName = FieldName (ASCII ByteString) deriving (Eq, Show)
data FieldValue = FieldValue (ASCII ByteString) deriving (Eq, Show)

-- Relude at play here with a type alias
data Body = Body LByteString deriving (Eq, Show)

-- Exercises!
-- 18. Construct some values
-- Where we try to use the above types to represent a pair of example
-- messages
helloRequest :: Request
helloRequest = Request start [host, lang] Nothing
  where
    start = RequestLine
              (Method [A.string|GET|])
              (RequestTarget [A.string|/hello.txt|])
              (Version Digit1 Digit1)
    host = Field
             (FieldName [A.string|Host|])
             (FieldValue [A.string|www.example.com|])
    lang = Field
             (FieldName [A.string|Accept-Language|])
             (FieldValue [A.string|en, mi|])

helloResponse :: Response
helloResponse = Response respStatus [ctype, len] body
  where
    respStatus = StatusLine
                   (Version Digit1 Digit1)
                   (StatusCode Digit2 Digit0 Digit0)
                   (Just $ ReasonPhrase [A.string|OK|])
    ctype = Field
              (FieldName [A.string|Content-Type|])
              (FieldValue [A.string|text/plain; charset=us-ascii|])
    len = Field
            (FieldName [A.string|Content-Length|])
            (FieldValue [A.string|6|])
    body = Just $ Body [A.string|Hello!|]

-- 19. Infinite byte strings
-- Find a function for constructing a lazy bytestring out of an infinite
-- string that does not crash GHCi.
-- Done, it was `LBS.cycle $ LBS.pack [Word8-literals]`
-- But the example was more elegant, so...
infiniteLaughs :: LByteString
infiniteLaughs = LBS.cycle [A.string|ha|]

laugh :: Int64 -> LByteString
laugh n = LBS.take n infiniteLaughs

-- Chapter 7: Encoding
--
-- Where we turn the above data types to actual ByteString responses
-- Let's start with a bit of building, shall we? First things first, good old
-- Monoid instance for Text (not a builder)
sayHello :: Text -> Text
sayHello name = T.pack "Hello, " <> name <> T.pack "!"

-- And now, let us use a builder for this. Same thing, different means.
sayHelloWithBuilder :: Text -> Text
sayHelloWithBuilder name = LT.toStrict . TB.toLazyText $
  TB.fromString "Hello " <> TB.fromText name <> TB.fromString "!"

-- That does not look more convenient in the slightest. What's the catch?
-- Well you see, we can have far more source types this way, so we have at least
-- that going for us which is nice. Additionally, we may not always want to use
-- strict text anyway, given that LazyText might just be sufficient for our means.
-- Consider printing, for instance. We don't need to have all the text in one
-- place. Chunks around the memory is fine. Observe:
printHello :: Text -> IO ()
printHello name = LT.putStrLn . TB.toLazyText $
  TB.fromString "Hello " <> TB.fromText name <> TB.fromString "!"

-- But that hardly seems worth it. What gives?
-- Memory. Memory gives. Do a plain monoid and you have the name from the
-- functions in memory multiple times. Check the chapter for more details.
-- Full details are not there either, but you'll get the gist of it.
-- Anyway, let's do some speed measurement casually!
time :: IO () -> IO ()
time action = do
  a <- Time.getCurrentTime
  action
  b <- Time.getCurrentTime
  print $ Time.diffUTCTime b a

concatWithStrict :: Int -> Text
concatWithStrict n = fold . replicate n $ T.pack "a"

concatWithBuilder :: Int -> Text
concatWithBuilder n = LT.toStrict . TB.toLazyText .
  fold . replicate n $ TB.fromString "a"

-- And because Haskell is lazy we need to use these values somewhere
concatSpeedTest :: Int -> IO ()
concatSpeedTest n = do
  dir <- getDataDir
  time $ T.writeFile (dir </> "strict.txt") (concatWithStrict n)
  time $ T.writeFile (dir </> "builder.txt") (concatWithBuilder n)

-- Okay, we promised to build the responses, so let's get to it shall we?
-- We are going to get concatenating lots, so builders will be our friends.
encodeLineEnd :: BSB.Builder
encodeLineEnd = A.fromCharList crlf

encodeRequest :: Request -> BSB.Builder
encodeRequest (Request requestLine fields bodyMaybe) =
  encodeRequestLine requestLine
  <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
  <> encodeLineEnd
  <> optionallyEncode encodeBody bodyMaybe

encodeResponse :: Response -> BSB.Builder
encodeResponse (Response statusLine fields bodyMaybe) =
  encodeStatusLine statusLine
  <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
  <> encodeLineEnd
  <> optionallyEncode encodeBody bodyMaybe

repeatedlyEncode :: (a -> BSB.Builder) -> [a] -> BSB.Builder
-- Okay this is quite different from the original suggestion, but I do like it
-- It is also mentioned in the material a bit later, so the latter inclusion
-- makes even more sense
repeatedlyEncode = foldMap
-- Including book example for reference:
--   fun enc xs = fold (map enc xs)

optionallyEncode :: (a -> BSB.Builder) -> Maybe a -> BSB.Builder
optionallyEncode enc (Just x) = enc x
optionallyEncode _ Nothing = mempty
-- What I didn't originally notice is that this, too, can be a foldMap
-- Makes sense, given how Maybe monoid folds.

encodeRequestLine :: RequestLine -> BSB.Builder
encodeRequestLine (RequestLine method target version) =
  encodeMethod method <> A.fromCharList [A.Space]
  <> encodeRequestTarget target <> A.fromCharList [A.Space]
  <> encodeVersion version <> encodeLineEnd

encodeMethod :: Method -> BSB.Builder
encodeMethod (Method x) = BSB.byteString $ A.lift x

encodeRequestTarget :: RequestTarget -> BSB.Builder
encodeRequestTarget (RequestTarget trg) = BSB.byteString $ A.lift trg

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine version code reason) =
  encodeVersion version <> A.fromCharList [A.Space]
  <> encodeStatusCode code <> A.fromCharList [A.Space]
  <> optionallyEncode encodeReasonPhrase reason
  <> encodeLineEnd

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) = A.fromDigitList [x,y,z]

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase (ReasonPhrase phr) = BSB.byteString $ A.lift phr

encodeVersion :: Version -> BSB.Builder
encodeVersion (Version x y) =
  [A.string|HTTP/|] <> A.fromDigitList [x] <> [A.string|.|] <> A.fromDigitList [y]

-- Exercises!
--
-- 20. Field encoding
-- You know the drill.
encodeField :: Field -> BSB.Builder
encodeField (Field name content) =
  encodeFieldName name <> [A.string|: |]
  <> encodeFieldValue content

encodeFieldName :: FieldName -> BSB.Builder
encodeFieldName (FieldName fname) = BSB.byteString $ A.lift fname

encodeFieldValue :: FieldValue -> BSB.Builder
encodeFieldValue (FieldValue content) = BSB.byteString $ A.lift content

-- 21. Body encoding
encodeBody :: Body -> BSB.Builder
encodeBody (Body bd) = BSB.lazyByteString bd

-- 22. Encoding test
-- Now did they work? Let's test this out! In `cabal repl`!
-- We cannot exactly measure this with the earlier string representations
-- since they are fundamentally different in content, due to things like
-- user agent fields etc. being involved in the strings.
-- But we do get reasonable looking values out! And while the request is what
-- it is, the response works fine!


-- Chapter 8: Responding
--
-- Let's begin with a classic hit counter
countHelloAscii :: Natural -> ASCII LByteString
countHelloAscii count = [A.string|Hello!|] <> A.fromCharList crlf <> case count of
    0 -> [A.string|This page has never been viewed.|]
    1 -> [A.string|This page has been viewed 1 time.|]
    _ -> [A.string|This page has been viewed |] <>
          A.showIntegralDecimal count <> [A.string| times.|]

data Status = Status StatusCode (Maybe ReasonPhrase)

ok :: Status
ok = Status (StatusCode Digit2 Digit0 Digit0) (Just $ ReasonPhrase [A.string|OK|])

status :: Status -> StatusLine
status (Status code phrase) = StatusLine http_1_1 code phrase

http_1_1 :: Version
http_1_1 = Version Digit1 Digit1

contentType :: FieldName
contentType = FieldName [A.string|Content-Type|]

plainAscii :: FieldValue
plainAscii = FieldValue [A.string|text/plain; charset=us-ascii|]

contentLength :: FieldName
contentLength = FieldName [A.string|Content-Length|]

asciiOk :: ASCII LByteString -> Response
asciiOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = Field contentType plainAscii
    len = Field contentLength (bodyLengthValue body)
    body = Body $ A.lift str

bodyLengthValue :: Body -> FieldValue
bodyLengthValue (Body x) = FieldValue . A.showIntegralDecimal $ LBS.length x

sendResponse :: Socket -> Response -> IO ()
sendResponse s r = Net.sendLazy s . BSB.toLazyByteString $ encodeResponse r

stuckCountingServer :: IO ()
stuckCountingServer = serve @IO HostAny "8000" \(s, _) -> do
  let count = 0 -- to-do!
  sendResponse s . asciiOk $ countHelloAscii count

-- Exercises!
-- 23. Read the header
-- Curl usually only gives us the response body. We want to see our server is
-- actually doing what we wanted it to, so let's see the header too.
-- The trick is to dump headers via curl to stdout. We did it!

-- 24. Overflow
-- We need to break the following:
mid :: Word8 -> Word8 -> Word8
mid x y = div (x+y) 2
-- What about a couple values over the halfway point? Yea, that'll do.
-- What about fixing it?
fixdMid :: Word8 -> Word8 -> Word8
fixdMid x y = fromInteger $ (toInteger x + toInteger y) `div` 2


-- Chapter 9: Content Types
-- Oh boy here we go, to the wild wild land of MIME types.
plainUtf8 :: FieldValue
plainUtf8 = FieldValue [A.string|text/plain; charset=utf-8|]

htmlUtf8 :: FieldValue
htmlUtf8 = FieldValue [A.string|text/html; charset=utf-8|]

json :: FieldValue
json = FieldValue [A.string|application/json|]

countHelloText :: Natural -> LText
countHelloText count = TB.toLazyText $
  TB.fromString "Hello! \9835\r\n" <>
  case count of
    0 -> TB.fromString "This page has never been viewed."
    1 -> TB.fromString "This page has been viewed 1 time."
    _ -> TB.fromString "This page has been viewed " <>
         TB.decimal count <> TB.fromString " times."

-- The headers are still ascii, but now we get to do the body in another
-- content type because we tell the responder it is UTF-8 now :)
textOk :: LText -> Response
textOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = Field contentType plainUtf8
    len = Field contentLength (bodyLengthValue body)
    body = Body $ LT.encodeUtf8 str

stuckCountingServerText :: IO ()
stuckCountingServerText = serve HostAny "8000" \(s, _) -> do
  let count = 0 -- to-do!
  sendResponse s . textOk $ countHelloText count

-- Here we could skip past HTML and JSON if we were not interested. I choose
-- to be, so here we go!
countHelloHtml :: Natural -> Html
countHelloHtml count = HTML.docType <> htmlDocument
  where
    htmlDocument = HTML.html $ documentMetadata <> documentBody

    documentMetadata = HTML.head titleHtml
    titleHtml = HTML.title $ toHtml "My great web page"

    documentBody = HTML.body $ greetingHtml <> HTML.hr <> hitCounterHtml
    greetingHtml = HTML.p $ toHtml "Hello! \9835"
    hitCounterHtml = HTML.p $ case count of
      0 -> toHtml "This page has never been viewed."
      1 -> toHtml "This page has been viewed 1 time."
      _ -> toHtml "This page has been viewed " <>
           -- look, a type application! Just so we can be sure we get it right.
           toHtml @Natural count <> toHtml " times."

-- And now that we have the full HTML thing we *could* now turn it to an UTF-8
-- bytestring of the lazy sort via `Text.Blaze.Html.Renderer.Utf8.renderHtml`.
-- Apparently we are not doing so here though, as it is JSON time!

countHelloJson :: Natural -> J.Value
countHelloJson count = toJSON $ J.KeyMap.fromList [greetingJson, hitsJson]
  where
    greetingJson = (J.Key.fromString "greeting", toJSON "Hello! \9835")

    hitsJson = (J.Key.fromString "hits",
      toJSON $ J.KeyMap.fromList [numberJson, messageJson])

    numberJson = (J.Key.fromString "count", toJSON count)
    messageJson = (J.Key.fromString "message", toJSON $ countHelloText count)
-- We could make this less legible and more terse. Observe:
countHelloJsonTerse :: Natural -> J.Value
countHelloJsonTerse count = J.object [
  fromString "greeting" .= fromString @Text "Hello! \9835",
  fromString "hits" .= J.object [
    fromString "count" .= count,
    fromString "message" .= countHelloText count
    ]
  ]

jsonOk :: J.Value -> Response
jsonOk str = Response (status ok) [typ, len] (Just body)
  where
    typ = Field contentType json
    len = Field contentLength $ bodyLengthValue body
    body = Body $ J.encode str


-- Exercises!
-- 25. HTML in the browser
-- Complete the following two functions and test them in browser
htmlOk :: Html -> Response
htmlOk content = Response (status ok) [typ, len] (Just body)
  where
    typ = Field contentType htmlUtf8
    len = Field contentLength $ bodyLengthValue body
    body = Body $ renderHtml content

stuckCountingServerHtml :: IO ()
stuckCountingServerHtml = serve HostAny "8000" \(s, _) -> do
  let count = 0 -- to-do!
  sendResponse s . htmlOk $ countHelloHtml count

-- 26. Type Ambiquity
-- What if we were to remove the `@Text` annotation from the terse JSON
-- example, I wonder? Let's see what the error message says and what does it
-- mean to us, if anything!
--
-- Well, it clearly say the type is ambiguous. Unable to figure out what
-- `isString` type it should be, since many JSON content types fit in the
-- field's content. Maybe it is a J.Value? Or a J.KeyMap? No, no it is not.
-- But the machine cannot tell.

-- 27. Encoding with class
-- Well now, we could always use a typeclass based type conversion for
-- bytestring. Let's soo how it looks like and why we should or should not do
-- such a thing!
class Encode a where
  encode :: a -> BSB.Builder

instance Encode Request where
  encode = encodeRequest

instance Encode Response where
  encode = encodeResponse

instance Encode Integer where
  encode = BSB.integerDec

instance Encode Int64 where
  encode = BSB.int64Dec

-- The following two instances rely on the two instances after them. This is
-- a brilliant idea that certainly does not make anything harder to understand.
-- It certainly does reduce repeated code though.
instance Encode Text where
  encode = encode . T.encodeUtf8

instance Encode LText where
  encode = encode . LT.encodeUtf8

instance Encode ByteString where
  encode = BSB.byteString

instance Encode LByteString where
  encode = BSB.lazyByteString

instance Encode BSB.Builder where
  encode = id


-- Chapter 10: Change
-- Where we learn how to make the NUMBER GO UP
--
-- The era of stuck counters is over. It is time for side effects and
-- maintaining state!

countingServer :: IO ()
countingServer = do
  hitCounter <- atomically $ newTVar @Natural 0
  serve @IO HostAny "8000" \(s, _) -> do
    count <- atomically $ increment hitCounter
    sendResponse s . textOk $ countHelloText count

increment :: TVar Natural -> STM Natural
increment hitCounter = do
  oldCount <- readTVar hitCounter
  let newCount = oldCount + 1
  writeTVar hitCounter newCount
  return newCount


-- Exercises!
--
-- 28. Interleaving
-- We shall go ahead and create a non-atomic version of increment. Surely this
-- will work fine with no corner cases appearing from the woodwork :)
incrementNotAtomic :: TVar Natural -> IO Natural
incrementNotAtomic hitCounter = do
  oldCount <- readTVarIO hitCounter
  let newCount = oldCount + 1
  atomically $ writeTVar hitCounter newCount
  return newCount

-- Oh look what might this be up to? Not sleuthing around for bugs I'm sure...
testIncrement :: (TVar Natural -> IO a) -> IO Natural
testIncrement inc = do
  x <- atomically $ newTVar @Natural 0
  Async.replicateConcurrently_ 10 (replicateM 1_000 $ inc x)
  readTVarIO x
-- Oh no! It turned out there were bugs regarding concurrency after all! Egads!

-- 29. Times gone by
-- Instead of a hit counter, we now implement a server that responds with the
-- time it had been left idling without requests in between.
timingServer :: IO ()
timingServer = do
  timeNow <- Time.getCurrentTime
  timer <- newTVarIO timeNow
  serve @IO HostAny "8000" \(s, _) -> do
    currentTime <- Time.getCurrentTime
    diffTime <- atomically $ requestDiffTime timer currentTime
    sendResponse s . textOk . LT.pack $ show diffTime

requestDiffTime
  :: TVar Time.UTCTime
  -> Time.UTCTime
  -> STM Time.NominalDiffTime
requestDiffTime timer currentTime = do
  oldTime <- readTVar timer
  let diffTime = Time.diffUTCTime currentTime oldTime
  writeTVar timer currentTime
  return diffTime
-- We will note that while the book asked for a Maybe type here and that there
-- have been no previous requests to the server so we can't really calculate
-- that the spec was not clear on that front so I elected to treat starting of
-- the server as the first request. It does not particularly contribute to the
-- lesson to go through the Maybe, so I did not to rewrite based on the model
-- solution.


-- Chapter 11: Streaming
-- Despite the pun potential, the completion of this chapter was not available
-- as a live-feed on the internet.
--
-- Where we learn how to not force the entire response into memory.
-- As in, not do what we are going to do next.
hContentsGetResponse :: Handle -> IO Response
hContentsGetResponse h = do
  fileContent <- liftIO $ LBS.hGetContents h
  let body = Just $ Body fileContent
  return $ Response (status ok) [] body

fileStrict :: IO ()
fileStrict = do
  dir <- getDataDir
  serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
    (_, h) <- binaryFileResource (dir </> "stream.txt") ReadMode
    r <- liftIO $ hContentsGetResponse h
    liftIO $ sendResponse s r
-- Now that we are here, if our files remained forever small this'd be fine.
-- Alas, they do not, so we need to be a touch more sophisticated.
--
-- Some RFC reading later, we arrive at...
helloResponseStringChunked :: ByteString
helloResponseStringChunked =
  line [A.string|HTTP/1.1 200 OK|] <>
  line [A.string|Content-Type: text/plain; charset=us-ascii|] <>
  line [A.string|Transfer-Encoding: chunked|] <>
  line [A.string||] <>
  line [A.string|2|] <> line [A.string|He|] <>
  line [A.string|4|] <> line [A.string|llo!|] <>
  line [A.string|0|] <> line [A.string||]

-- Anyway, I don't imagine we want to be writing chunks manually any more.
-- What did we do before? that's right, we created data types:
data Chunk = Chunk ChunkSize ChunkData
newtype ChunkData = ChunkData ByteString
newtype ChunkSize = ChunkSize Natural

-- Now that we have them, let's use them.
dataChunk :: ChunkData -> Chunk
dataChunk chunkData = Chunk (chunkDataSize chunkData) chunkData

chunkDataSize :: ChunkData -> ChunkSize
chunkDataSize (ChunkData bs) = case toIntegralSized @Int @Natural (BS.length bs) of
  Just n -> ChunkSize n
  Nothing -> error $ T.pack "BS.length is always Natural"

-- And of course, we need to be able to encode them.
encodeChunk :: Chunk -> BSB.Builder
encodeChunk (Chunk chunkSize chunkData) =
  encodeChunkSize chunkSize <> encodeLineEnd <>
  encodeChunkData chunkData <> encodeLineEnd

encodeChunkSize :: ChunkSize -> BSB.Builder
encodeChunkSize (ChunkSize n) = A.showIntegralHexadecimal A.LowerCase n

encodeLastChunk :: BSB.Builder
encodeLastChunk = encodeChunkSize (ChunkSize 0) <> encodeLineEnd

encodeChunkData :: ChunkData -> BSB.Builder
encodeChunkData (ChunkData cdata) = BSB.byteString cdata

transferEncoding :: FieldName
transferEncoding = FieldName [A.string|Transfer-Encoding|]

chunked :: FieldValue
chunked = FieldValue [A.string|chunked|]

transferEncodingChunked :: Field
transferEncodingChunked = Field transferEncoding chunked

-- And now we make the magic happen:
fileStreaming :: IO ()
fileStreaming = do
  dir <- getDataDir
  serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
    (_, h) <- binaryFileResource (dir </> "stream.txt") ReadMode
    liftIO do
      sendBSB s . encodeStatusLine $ status ok
      sendBSB s $ encodeFieldList [transferEncodingChunked]
      repeatUntil (BS.hGetSome h 1_024) BS.null \chunk ->
        sendBSB s . encodeChunk . dataChunk $ ChunkData chunk
      sendBSB s encodeLastChunk

sendBSB :: Socket -> BSB.Builder -> IO ()
sendBSB s bs = Net.sendLazy s $ BSB.toLazyByteString bs

encodeFieldList :: [Field] -> BSB.Builder
encodeFieldList xs =
  repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) xs
  <> encodeLineEnd


-- Exercises!
-- 30. Tripping at the finish line
-- Move fast and break things! Not enough line breaks? Too many? Let's see how
-- we can break `fileStreaming` with great skill and purpose.
--
-- Seems that too many lines is no problem at all, but leaving some out has
-- curl acting all confused about the connection closing while there was still
-- outstanding data to be read, which makes sense as we break the protocol spec.

-- 31. Infinite response
-- You know, the chunks don't have to end, ever. What if...
dawnMachineServer :: IO ()
dawnMachineServer = do
  serve @IO HostAny "8000" \(s, _) -> do
    sendBSB s . encodeStatusLine $ status ok
    sendBSB s $ encodeFieldList [transferEncodingChunked]
    forever do
      sendBSB s . encodeChunk . dataChunk $ ChunkData [A.string|E SUN THE SUN TH|]
-- Supremacy: Dawn Machine is increasing...


-- Chapter 12: Pipes
-- Where we fix some of the sacrifices previously done in the name of convenience
--
-- So, in the previous chapter, we wrote a program that responds, but we did not
-- define what a response actually is. Yikes. We really ought to do something
-- about that, shouldn't we?
newtype MaxChunkSize = MaxChunkSize Int

fileStreaming2 :: IO ()
fileStreaming2 = do
  dir <- getDataDir
  serve @IO HostAny "8000" \(s, _) -> runResourceT @IO do
    (_, h) <- binaryFileResource (dir </> "stream.txt") ReadMode
    let r = hStreamingResponse h (MaxChunkSize 1_024)
    liftIO $ sendStreamingResponse s r

data StreamingResponse = StreamingResponse StatusLine [Field] (Maybe ChunkedBody)
newtype ChunkedBody = ChunkedBody (Producer Chunk IO ())

-- And now to get some understanding of how to use these producers, consumers,
-- pipes and whatnot
demoProducer :: Producer Text IO ()
demoProducer = do
  yield $ T.pack "one, "
  yield $ T.pack "two, "
  yield $ T.pack "three"
  replicateM_ 10 do
    liftIO $ threadDelay 100_000
    yield $ T.pack "."
  yield $ T.pack "GO"

putTextConsumer :: Consumer Text IO ()
putTextConsumer =
  forever do
    x <- await
    putText x

hStreamingResponse :: Handle -> MaxChunkSize -> StreamingResponse
hStreamingResponse h maxChunkSize = StreamingResponse statusLine fields (Just body)
  where
    statusLine = status ok
    fields = [transferEncodingChunked]
    body = chunkedBody $ hChunks h maxChunkSize

hChunks :: Handle -> MaxChunkSize -> Producer ByteString IO ()
-- Original form:
--
-- hChunks h (MaxChunkSize mcs) = go
--   where
--     go = do
--       chunk <- liftIO $ BS.hGetSome h mcs
--       if BS.null chunk
--         then return ()
--         else do
--           yield chunk
--           go
hChunks h (MaxChunkSize mcs) = produceUntil (BS.hGetSome h mcs) BS.null

stringsToChunks :: Pipe ByteString Chunk IO ()
stringsToChunks =
  forever do
    bs <- await
    yield . dataChunk $ ChunkData bs

chunkedBody :: Producer ByteString IO () -> ChunkedBody
chunkedBody xs = ChunkedBody $ xs >-> stringsToChunks

encodeStreamingResponse :: StreamingResponse -> Producer ByteString IO ()
encodeStreamingResponse (StreamingResponse statusLine headers bodyMaybe) = do
  yield $ encodeStatusLine statusLine
  yield $ encodeFieldList headers

  for_ bodyMaybe \(ChunkedBody body) -> do
    body >-> encodeChunks
    yield encodeLastChunk
    yield $ encodeFieldList []

  >-> build

encodeChunks :: Pipe Chunk BSB.Builder IO ()
encodeChunks =
  forever do
    chunk <- await
    yield $ encodeChunk chunk

build :: Pipe BSB.Builder ByteString IO ()
build = forever do
  bsb <- await
  let chunks = LBS.toChunks $ BSB.toLazyByteString bsb
  for_ chunks yield

sendStreamingResponse :: Socket -> StreamingResponse -> IO ()
sendStreamingResponse s r = runEffect @IO $ encodeStreamingResponse r >-> toSocket s

-- Exercises!
--
-- 32. Into the socket
-- Write the consumer `toSocket` so that te above function works
toSocket :: Socket -> Consumer ByteString IO ()
toSocket s = forever do
  bs <- await
  liftIO $ Net.send s bs

-- 33. Produce until
-- You know the drill! We have done these generic "do something until x"
-- functions a couple times now. Time to write a Producer!
produceUntil :: IO chunk -> (chunk -> Bool) -> Producer chunk IO ()
produceUntil chunker isEnd = go
  where
    go = do
      chunk <- liftIO chunker
      if isEnd chunk
        then return ()
        else
          yield chunk >> go
-- And now that we are here, we'll go ahead and rewrite `hChunks`. See above!

-- 34. File copying
-- In chapter 3, we had `copyGreetingFile`. Let's make that a stream!
copyGreetingStream :: IO ()
copyGreetingStream = runResourceT do
  dir <- liftIO getDataDir
  (_, h1) <- binaryFileResource (dir </> "greeting.txt") ReadMode
  (_, h2) <- binaryFileResource (dir </> "greeting2.txt") WriteMode
  liftIO $ hCopy h1 h2

-- This `hCopy` here is the part we supply (mostly) ourselves
hCopy :: Handle -> Handle -> IO ()
hCopy source destination = runEffect @IO (producer >-> consumer)
  where
    producer = hChunks source $ MaxChunkSize 1_024
    consumer = forever do
      chunk <- await
      liftIO $ BS.hPutStr destination chunk

-- 35. Copying to multiple destinations
-- Repeat previous, but with some chest hair!
fileCopyMany :: FilePath -> [FilePath] -> IO ()
fileCopyMany source destinations = runResourceT do
  (_, hSource) <- binaryFileResource source ReadMode
  hDestinations <- forM destinations \fp -> do
    (_, h) <- binaryFileResource fp WriteMode
    return h
  liftIO $ hCopyMany hSource hDestinations

hCopyMany :: Handle -> [Handle] -> IO ()
hCopyMany source destinations = runEffect @IO (producer >-> consumer)
  where
    producer = hChunks source $ MaxChunkSize 1_024
    consumer = forever do
      chunk <- await
      liftIO $ mapM (flip BS.hPutStr chunk) destinations


-- Chapter 13: Parsing
--
-- We probably should also read the messages we are sent, huh?
newtype ResourceName = ResourceName Text deriving (Eq, Ord)
newtype ResourceMap = ResourceMap (Map ResourceName FilePath)

resourceMap :: FilePath -> ResourceMap
resourceMap dir = ResourceMap $ Map.fromList
  [ (streamResource, dir </> "stream.txt"),
    (readResource, dir </> "read.txt")
  ]

streamResource :: ResourceName
streamResource = ResourceName $ T.pack "/stream"

readResource :: ResourceName
readResource = ResourceName $ T.pack "/read"

-- Attoparsec tutorial time! Let's do some examples before diving into our deep
-- end of things, to get our bearings and all.
-- Most of the demoing was in REPL, but these help if one wants to rerun them :)
countString :: ByteString
countString = [A.string|one-two-three-four|]

takeTwoWords :: Parser (ByteString, ByteString)
takeTwoWords = do
  a <- P.takeWhile A.isLetter
  _ <- P.string $ A.fromCharList [A.HyphenMinus]
  b <- P.takeWhile A.isLetter
  return (a,b)

-- Back to it!
spaceParser :: Parser ByteString
spaceParser = P.string $ A.fromCharList [A.Space]

lineEndParser :: Parser ByteString
lineEndParser = P.string $ A.fromCharList crlf

requestLineParser :: Parser RequestLine
requestLineParser = do
  method <- methodParser <?> "Method"
  _ <- spaceParser <|> fail "Method should be followed by a space"
  target <- requestTargetParser <?> "Target"
  _ <- spaceParser <|> fail "Target should be followed by a space"
  version <- versionParser <?> "Version"
  _ <- lineEndParser <|> fail "Version should be followed by end of line"
  return $ RequestLine method target version

methodParser :: Parser Method
-- We do a little trolling (this was a more approachable `do` block)
methodParser = Method <$> tokenParser

tokenParser :: Parser (ASCII ByteString)
tokenParser = do
  bs <- P.takeWhile1 isTchar
  case A.convertStringMaybe bs of
    Just asciiBS -> return asciiBS
    Nothing -> fail "Non-ASCII tchar"

isTchar :: Word8 -> Bool
isTchar c = elem c tcharSymbols || A.isDigit c || A.isLetter c

tcharSymbols :: [Word8]
tcharSymbols = A.fromCharList [ A.ExclamationMark, A.NumberSign, A.DollarSign,
  A.PercentSign, A.Ampersand, A.Apostrophe, A.Asterisk, A.PlusSign, A.HyphenMinus,
  A.FullStop, A.Caret, A.Underscore, A.GraveAccent, A.VerticalLine, A.Tilde ]

-- We will leave this slightly incomplete, i.e. not fully spec compliant as this
-- is a tutorial book, not a painstaking effort to write a full HTTP library
requestTargetParser :: Parser RequestTarget
requestTargetParser = do
  bs <- P.takeWhile1 A.isVisible
  case A.convertStringMaybe bs of
    Just asciiBS -> return $ RequestTarget asciiBS
    Nothing -> fail "Non-ASCII vchar"

versionParser :: Parser Version
versionParser = do
  _ <- P.string [A.string|HTTP|] <|> fail "Should start with HTTP"
  _ <- P.string (A.fromCharList [A.Slash]) <|> fail "HTTP should be followed by a slash"
  x <- digitParser <?> "First digit"
  _ <- P.string (A.fromCharList [A.FullStop]) <|> fail "Major and minor version numbers should be separated by a full stop"
  y <- digitParser <?> "Second digit"
  return $ Version x y

digitParser :: Parser Digit
digitParser = do
  x <- P.anyWord8
  case A.word8ToDigitMaybe x of
    Just d -> return d
    Nothing -> fail "0-9 expected"
-- Then we added context to `requestLineParser` via `(<?>)` and `(<|>)`

-- Anyway, time to actually do the incremental parsing thing we picked
-- the Attoparsec library for!
newtype Input = Input (P.RestorableInput IO ByteString)

parseFromSocket :: Socket -> MaxChunkSize -> IO Input
parseFromSocket s (MaxChunkSize mcs) = Input <$> P.newRestorableIO (S.recv s mcs)

readRequestLine :: Input -> IO RequestLine
readRequestLine (Input i) = do
  result <- P.parseAndRestore i (requestLineParser <?> "Request line")
  case result of
    Left parseError -> fail $ P.showParseError parseError
    Right requestLine -> return requestLine

resourceServer :: IO ()
resourceServer = do
  dir <- getDataDir
  let resources = resourceMap dir
  let maxChunkSize = MaxChunkSize 1_024
  serve @IO HostAny "8000" \(s, _) -> serveResourceOnce resources maxChunkSize s

serveResourceOnce :: ResourceMap -> MaxChunkSize -> Socket -> IO ()
serveResourceOnce resources maxChunkSize s = runResourceT @IO do
  i <- liftIO $ parseFromSocket s maxChunkSize
  RequestLine _ target _ <- liftIO $ readRequestLine i
  filePath <- liftIO $ getTargetFilePath resources target
  (_, h) <- binaryFileResource filePath ReadMode
  let r = hStreamingResponse h maxChunkSize
  liftIO $ sendStreamingResponse s r

getTargetFilePath :: ResourceMap -> RequestTarget -> IO FilePath
getTargetFilePath rs target =
  case targetFilePathMaybe rs target of
    Nothing -> fail "not found"
    Just fp -> return fp

targetFilePathMaybe :: ResourceMap -> RequestTarget -> Maybe FilePath
targetFilePathMaybe (ResourceMap rs) (RequestTarget target) = Map.lookup r rs
  where
    r = ResourceName $ A.asciiByteStringToText target


-- Exercises!
--
-- 36. Parsing parentheses
-- It is time to bring `unParen` (ex. 6) to this day and age of parsers!
parenParser :: Parser ByteString
parenParser = do
  _ <- parseOpeningParen
  content <- parseNonParen
  _ <- parseClosingParen
  return content
    where
      parseOpeningParen = P.word8 $ A.toWord8 A.LeftParenthesis
      parseNonParen = P.takeTill (== A.toWord8 A.RightParenthesis)
      parseClosingParen = P.word8 $ A.toWord8 A.RightParenthesis
-- There are still very much not nice corner cases here, but it matches the
-- model solution and clears the example so who am I to complain?

-- 37. To digit, maybe
-- Rewrite `digitParser` without partial functions

-- 38. Better parse errors
-- We revised Version parsing to be nicer

-- 39. Status line
-- Let's write a parser for responses too, for a laugh
statusLineParser :: Parser StatusLine
statusLineParser = do
  version <- versionParser
  _ <- spaceParser
  statusCode <- statusCodeParser
  _ <- spaceParser
  reason <- reasonPhraseParser
  _ <- lineEndParser
  return $ StatusLine version statusCode reason
    where
      statusCodeParser = do
        x <- digitParser
        y <- digitParser
        z <- digitParser
        return $ StatusCode x y z
      reasonPhraseParser = do
        potentialPhrase <- P.takeWhile isWithinReason
        if BS.null potentialPhrase
          then return Nothing
          else do
            x <- A.convertStringOrFail potentialPhrase
            return . Just $ ReasonPhrase x

      isWithinReason :: Word8 -> Bool
      isWithinReason c = c `elem` map A.fromChar [A.HorizontalTab, A.Space] || A.isVisible c

statusLineParseTest :: StatusLine -> Maybe String
statusLineParseTest statusLine = do
  let encodedSL = BS.toStrict . BSB.toLazyByteString $ encodeStatusLine statusLine
  case P.parseOnlyStrict encodedSL (statusLineParser <* P.endOfInput) of
    Left err -> Just $ P.showParseError err
    Right x' | statusLine /= x' -> Just $ show x'
    Right _ -> Nothing
-- I did not enjoy this exercise. There were several gotchas involved in it
-- that had not been discussed in the chapter.

-- 40. P.string
-- Suppose we did not have `P.string`. Could we make do?
scuffedStringParser :: ByteString -> Parser ByteString
scuffedStringParser bs
  | BS.null bs = return BS.empty
  | otherwise =
      case BS.uncons bs of
        Nothing -> fail "Predicate longer than string"
        Just (w8,req) -> do
          w8' <- P.anyWord8
          if w8 == w8'
            then BS.cons w8' <$> scuffedStringParser req
            else fail "Did not meet predicate!"


-- Chapter 14: Errors
-- Where we admit we should tell the client if they, or worse yet us, have done
-- something wrong in the process of replying.
badRequest :: Status
badRequest = Status
  (StatusCode Digit4 Digit0 Digit0)
  (Just $ ReasonPhrase [A.string|Bad request|])

notFound :: Status
notFound = Status
  (StatusCode Digit4 Digit0 Digit4)
  (Just $ ReasonPhrase [A.string|Not found|])

serverError :: Status
serverError = Status
  (StatusCode Digit5 Digit0 Digit0)
  (Just $ ReasonPhrase [A.string|Server error|])

methodNotAllowed :: Status
methodNotAllowed = Status
  (StatusCode Digit4 Digit0 Digit5)
  (Just $ ReasonPhrase [A.string|Method not allowed|])

versionNotSupported :: Status
versionNotSupported = Status
  (StatusCode Digit5 Digit0 Digit5)
  (Just $ ReasonPhrase [A.string|HTTP version not supported|])

textResponse :: Status -> [Field] -> LText -> Response
textResponse s additionalFields bodyText =
  Response (status s)
           ([typ, len] <> additionalFields)
           (Just body)
  where
    typ = Field contentType plainUtf8
    len = Field contentLength $ bodyLengthValue body
    body = Body $ LT.encodeUtf8 bodyText

newtype LogEvent = LogEvent LText

printLogEvent :: LogEvent -> IO ()
printLogEvent (LogEvent x) = LT.putStrLn x

data Error = Error (Maybe Response) [LogEvent]

handleRequestError :: (LogEvent -> IO b) -> Socket -> Error -> IO ()
handleRequestError log s (Error responseMaybe events) = do
  for_ events log
  for_ responseMaybe (sendResponse s)

requestParseError :: P.ParseError -> Error
requestParseError parseError = Error (Just response) [event]
  where
    response = textResponse badRequest [] message
    event = LogEvent message
    message = TB.toLazyText (
      TB.fromString "Malformed request: " <>
      TB.fromString (P.showParseError parseError))

notFoundError :: Error
notFoundError = Error (Just response) []
  where
    response = textResponse notFound [] message
    message = LT.pack "It just isnt there!"

methodError :: [Method] -> Error
methodError supportedMethods = Error (Just response) []
  where
    response = textResponse methodNotAllowed [allowField supportedMethods] LT.empty

allowField :: [Method] -> Field
allowField methods = Field (FieldName [A.string|Allow|]) value
  where
    value = FieldValue . commaList $ map (\(Method m) -> m) methods
    commaList xs = fold $ intersperse (A.fromCharList [A.Comma, A.Space]) xs

fileOpenError :: FilePath -> SomeException -> Error
fileOpenError filePath ex = Error (Just response) [event]
  where
    response = textResponse serverError [] $ LT.pack "Something went wrong."
    event = LogEvent . TB.toLazyText $
      TB.fromString "Failed to open file " <>
      TB.fromString (show filePath) <> TB.fromString ": " <>
      TB.fromString (displayException ex)

ungracefulError :: SomeException -> Error
ungracefulError e = Error Nothing [event]
  where
    event = LogEvent . LT.pack $ displayException e

handleIOExceptions :: IO (Either Error a) -> IO (Either Error a)
handleIOExceptions action = do
  result <- tryAny action
  case result of
    Left e -> return . Left $ ungracefulError e
    Right x -> return x

-- And now, let us handle them errors unlike in the past!
getTargetFilePathX :: ResourceMap -> RequestTarget -> Either Error FilePath
getTargetFilePathX rs t =
  case targetFilePathMaybe rs t of
    Nothing -> Left notFoundError
    Just fp -> Right fp

readRequestLineX :: Input -> IO (Either Error RequestLine)
readRequestLineX i = readRequestPart i "Request line" requestLineParser

-- Let's generalise the above a bit. Bound to be useful! At least we can
-- go ahead and refactor it to be all fancy-like next!
readRequestPart :: Input -> String -> Parser a -> IO (Either Error a)
readRequestPart (Input i) description p = do
  result <- liftIO (P.parseAndRestore i (p <?> description))
  case result of
    Left e -> return . Left $ requestParseError e
    Right requestLine -> return $ Right requestLine

binaryFileResourceX
  :: FilePath
  -> IOMode
  -> ResourceT IO (Either Error (ReleaseKey, Handle))
binaryFileResourceX fp mode = do
  result <- tryAny $ binaryFileResource fp mode
  case result of
    Left e -> return . Left $ fileOpenError fp e
    Right x -> return $ Right x

requireMethodX :: [Method] -> Method -> Either Error ()
requireMethodX supportedMethods x
  | x `elem` supportedMethods = Right ()
  | otherwise = Left $ methodError supportedMethods

-- This just was not in the book, did it myself ":D"
sendStreamingResponseX :: Socket -> StreamingResponse -> IO (Either Error ())
sendStreamingResponseX s r = do
  sent <- tryAny . runEffect @IO $ encodeStreamingResponse r >-> toSocket s
  case sent of
    Left e -> return . Left $ ungracefulError e
    Right () -> return $ Right ()

serveResourceOnceX :: ResourceMap -> MaxChunkSize -> Socket -> IO (Either Error ())
serveResourceOnceX resources maxChunkSize s =
  handleIOExceptions . runResourceT @IO $ runExceptT @Error @(ResourceT IO) do
    i <- liftIO $ parseFromSocket s maxChunkSize
    RequestLine method target version <- ExceptT . liftIO $ readRequestLineX i
    ExceptT . return $ requireMethodX [Method [A.string|GET|]] method
    ExceptT . return $ requireVersionX http_1_1 version
    fp <- ExceptT . return $ getTargetFilePathX resources target
    (_, h) <- ExceptT $ binaryFileResourceX fp ReadMode
    let r = hStreamingResponse h maxChunkSize
    ExceptT . liftIO $ sendStreamingResponseX s r

-- Exercises!
--
-- 41. Resource Server X
-- Let us write `resourceServerX`
resourceServerX :: IO ()
resourceServerX = do
  dir <- getDataDir
  let resources = resourceMap dir
  let maxChunkSize = MaxChunkSize 1_024
  unforkAsyncIO_ printLogEvent \log ->
    serve @IO HostAny "8000" \(s, _) -> do
      served <- serveResourceOnceX resources maxChunkSize s
      case served of
        Left e -> handleRequestError log s e
        Right _ -> return ()

-- 42. Check method and HTTP version
-- Write the function below, then add it and `requireMethodX` to
-- `serveResourceOnceX` for completeness
requireVersionX :: Version -> Version -> Either Error ()
requireVersionX supportedVersion requestVersion =
  if supportedVersion == requestVersion
    then Right ()
    else Left badRequestVersion

badRequestVersion :: Error
badRequestVersion = Error (Just response) []
  where
    response = textResponse versionNotSupported [] $ LT.pack "Sorry, HTTP/1.1 only"

-- 43. A sinister request
-- Where we make a request that sucks. A HTTP client that absolutely fails at
-- its job. A completely bungled up mess. The aim is to generate a response with
-- the status code 400.
sendSinisterRequest :: IO ()
sendSinisterRequest = runResourceT @IO do
  addrInfo <- liftIO $ resolve "8000" "localhost"
  (_, s) <- openAndConnect addrInfo
  Net.send s [A.string|POST /oispa kaljaa|]
  liftIO $ repeatUntilNothing (Net.recv s 1_024) BS.putStr
