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
data Request = Request RequestLine [Field] (Maybe Body)
data Response = Response StatusLine [Field] (Maybe Body)

data RequestLine = RequestLine Method RequestTarget Version
data Method = Method (ASCII ByteString)
data RequestTarget = RequestTarget (ASCII ByteString)
data Version = Version Digit Digit

data StatusLine = StatusLine Version StatusCode (Maybe ReasonPhrase)
data StatusCode = StatusCode Digit Digit Digit
data ReasonPhrase = ReasonPhrase (ASCII ByteString)

data Field = Field FieldName FieldValue
data FieldName = FieldName (ASCII ByteString)
data FieldValue = FieldValue (ASCII ByteString)

-- Relude at play here with a type alias
data Body = Body LByteString

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
helloResponse = Response status [ctype, len] body
  where
    status = StatusLine
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

-- This causes a shadowing warning
status :: Status -> StatusLine
status (Status code phrase) = StatusLine http_1_1 code phrase

http_1_1 :: Version
http_1_1 = Version Digit1 Digit1

-- And these would prefer to have type signatures
-- I might fix these in a later commit when the warnings annoy me enough
contentType = FieldName [A.string|Content-Type|]
plainAscii = FieldValue [A.string|text/plain; charset=us-ascii|]
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
