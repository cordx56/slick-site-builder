{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens (Profunctor (lmap), at, (?~))
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON, Value (Object, String), toJSON)
import Data.Aeson.KeyMap (union)
import Data.Aeson.Lens (_Object)
import qualified Data.Text as T
import Data.Time (
  UTCTime,
  defaultTimeLocale,
  formatTime,
  getCurrentTime,
  iso8601DateFormat,
  parseTimeOrError,
 )
import Data.Yaml (decodeFileEither)
import Development.Shake (
  Action,
  Verbosity (Verbose),
  copyFileChanged,
  forP,
  getDirectoryFiles,
  liftIO,
  readFile',
  shakeLintInside,
  shakeOptions,
  shakeVerbosity,
  writeFile',
 )
import Development.Shake.Classes (Binary)
import Development.Shake.FilePath (dropDirectory1, (-<.>), (</>))
import Development.Shake.Forward (cacheAction, shakeArgsForward)
import GHC.Generics (Generic)
import Slick (compileTemplate', convert, substitute)
import Slick.Pandoc (defaultHtml5Options, defaultMarkdownOptions, markdownToHTMLWithOpts)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPrint, stderr)
import Text.Pandoc (Extension (Ext_east_asian_line_breaks, Ext_emoji), ReaderOptions, def, extensionsFromList, readerExtensions)

---Config-----------------------------------------------------------------------

outputFolder :: FilePath
outputFolder = "docs/"

baseDir :: FilePath
baseDir = "site/"

templateDir :: FilePath
templateDir = baseDir </> "templates/"

metaFile :: FilePath
metaFile = baseDir </> "meta.yaml"

pandocReaderOptions :: ReaderOptions
pandocReaderOptions =
  def
    { readerExtensions =
        mconcat
          [ extensionsFromList [Ext_east_asian_line_breaks, Ext_emoji]
          , defaultOptions
          ]
    }
 where
  defaultOptions = readerExtensions defaultMarkdownOptions

-- Data models-------------------------------------------------------------------

withSiteMeta :: SiteMeta -> Value -> Value
withSiteMeta siteMeta (Object obj) = Object $ union obj siteMetaObj
 where
  Object siteMetaObj = toJSON siteMeta
withSiteMeta _ _ = error "only add site meta to objects"

data SiteMeta = SiteMeta
  { siteAuthor :: String
  , baseUrl :: String -- e.g. https://example.ca
  , siteTitle :: String
  -- , twitterHandle :: Maybe String -- Without @
  -- , githubUser :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON)

-- | Data for the index page
data IndexInfo = IndexInfo
  { posts :: [Post]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type Tag = String

-- | Data for a blog post
data Post = Post
  { title :: String
  , author :: Maybe String
  , content :: String
  , url :: String
  , date :: Maybe String
  , tags :: Maybe [Tag]
  , description :: Maybe String
  , image :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

data AtomData = AtomData
  { title :: String
  , domain :: String
  , author :: String
  , posts :: [Post]
  , currentTime :: String
  , atomUrl :: String
  }
  deriving (Generic, ToJSON, Eq, Ord, Show)

-- | given a list of posts this will build a table of contents
buildIndex :: SiteMeta -> [Post] -> Action ()
buildIndex siteMeta posts' = do
  indexT <- compileTemplate' (templateDir </> "base.html")
  let indexInfo = IndexInfo{posts = posts'}
      indexHTML = T.unpack $ substitute indexT (withSiteMeta siteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "index.html") indexHTML

-- Archive page builder
buildArchive :: SiteMeta -> [Post] -> Action ()
buildArchive siteMeta posts' = do
  indexT <- compileTemplate' (templateDir </> "archive.html")
  let indexInfo = IndexInfo{posts = posts'}
      indexHTML = T.unpack $ substitute indexT (withSiteMeta siteMeta $ toJSON indexInfo)
  writeFile' (outputFolder </> "archive.html") indexHTML

-- | Find and build all posts
buildPosts :: SiteMeta -> Action [Post]
buildPosts siteMeta = do
  pPaths <- getDirectoryFiles baseDir ["posts//*.md"]
  forP (map (baseDir </>) pPaths) $ buildPost siteMeta

{- | Load a post, process metadata, write it to output, then return the post object
Detects changes to either post content or template
-}
buildPost :: SiteMeta -> FilePath -> Action Post
buildPost siteMeta srcPath = cacheAction ("build" :: T.Text, srcPath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcPath
  postContent <- readFile' srcPath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTMLWithOpts pandocReaderOptions defaultHtml5Options . T.pack $ postContent
  let postUrl = T.pack . dropDirectory1 $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta siteMeta . withPostUrl $ postData
  template <- compileTemplate' (templateDir </> "base.html")
  writeFile' (outputFolder </> T.unpack postUrl) . T.unpack $ substitute template fullPostData
  convert fullPostData

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Action ()
copyStaticFiles = do
  filepaths <- getDirectoryFiles baseDir ["assets//*"]
  void $ forP filepaths $ \filepath ->
    copyFileChanged (baseDir </> filepath) (outputFolder </> dropDirectory1 filepath)

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
 where
  parsedTime =
    parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:%SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

buildFeed :: SiteMeta -> [Post] -> Action ()
buildFeed siteMeta posts' = do
  now <- liftIO getCurrentTime
  let atomData =
        AtomData
          { title = siteTitle siteMeta
          , domain = baseUrl siteMeta
          , author = siteAuthor siteMeta
          , posts = mkAtomPost <$> posts'
          , currentTime = toIsoDate now
          , atomUrl = "/atom.xml"
          }
  atomTempl <- compileTemplate' (templateDir </> "atom.xml")
  writeFile' (outputFolder </> "atom.xml") . T.unpack $ substitute atomTempl (toJSON atomData)
 where
  mkAtomPost :: Post -> Post
  mkAtomPost p = case date p of
    Just d -> p{date = Just $ formatDate d}
    Nothing -> p

{- | Specific build rules for the Shake system
  defines workflow to build the website
-}
buildRules :: SiteMeta -> Action ()
buildRules siteMeta = do
  allPosts <- buildPosts siteMeta
  -- buildIndex siteMeta allPosts
  buildArchive siteMeta allPosts
  -- buildFeed siteMeta allPosts
  copyStaticFiles

main :: IO ()
main = do
  metaEither <- decodeFileEither metaFile
  case metaEither of
    Left _ -> do
      hPrint stderr (metaFile ++ " read error")
      exitWith (ExitFailure 1)
    Right meta -> do
      let shOpts = shakeOptions{shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
      shakeArgsForward shOpts $ buildRules meta
