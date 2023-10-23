{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens (Profunctor (lmap), at, (?~))
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (Object, String), object, toJSON, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap as KeyMap (lookup, singleton, union)
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
  FilePattern,
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
import Development.Shake.FilePath (dropDirectory1, normaliseEx, splitPath, (-<.>), (</>))
import Development.Shake.Forward (cacheAction, shakeArgsForward)
import GHC.Generics (Generic)
import Slick (compileTemplate', convert, substitute)
import Slick.Pandoc (defaultHtml5Options, defaultMarkdownOptions, markdownToHTMLWithOpts)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPrint, stderr)
import Text.Pandoc (Extension (Ext_east_asian_line_breaks, Ext_emoji), ReaderOptions, def, extensionsFromList, readerExtensions)

---Config-----------------------------------------------------------------------

configFile :: FilePath
configFile = "config.yaml"

-- Data models-------------------------------------------------------------------

-- | Data for config
data Config = Config
  { site :: SiteConfig
  , srcDir :: FilePath
  , outputDir :: FilePath
  , templatesDir :: FilePath
  , rules :: [Rule]
  }
  deriving (Generic, FromJSON)

data SiteConfig = SiteConfig
  { baseUrl :: String
  , title :: String
  }
  deriving (Generic, FromJSON)

siteConfigToSiteMeta :: SiteConfig -> SiteMeta
siteConfigToSiteMeta (SiteConfig{baseUrl = u, title = t}) =
  SiteMeta
    { baseUrl = u
    , siteTitle = t
    }

data Rule = Rule
  { name :: Maybe String
  , action :: RuleAction
  , patterns :: [FilePattern]
  }
  deriving (Generic, FromJSON)

data RuleAction
  = CopyRule
      { name :: String
      }
  | BuildRule
      { name :: String
      , template :: FilePath
      }

instance FromJSON RuleAction where
  parseJSON (Object v) = case KeyMap.lookup "name" v of
    (Just "copy") -> CopyRule <$> v .: "name"
    (Just "build") ->
      BuildRule
        <$> v
        .: "name"
        <*> v
        .: "template"
    _ -> error "unknown rule action"
  parseJSON _ = error "action must be object"

-- | Pandoc options
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

withSiteMeta :: SiteMeta -> Value -> Value
withSiteMeta siteMeta (Object obj) = Object $ union obj siteMetaObj
 where
  siteMetaObj = case toJSON siteMeta of
    Object o -> o
    _ -> error "type error"
withSiteMeta _ _ = error "only add site meta to objects"

data SiteMeta = SiteMeta
  { baseUrl :: String -- e.g. https://example.ca
  , siteTitle :: String
  -- , siteAuthor :: String
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
  { title :: Maybe String
  , author :: Maybe String
  , content :: String
  , url :: String
  , date :: Maybe String
  , tags :: Maybe [Tag]
  , description :: Maybe String
  , image :: Maybe String
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

-- | Find and build all posts
builds :: Config -> [FilePattern] -> FilePath -> Action [Post]
builds config patts templ = do
  pPaths <- getDirectoryFiles (srcDir config) patts
  forP pPaths $ build config templ

{- | Load a post, process metadata, write it to output, then return the post object
Detects changes to either post content or template
-}
build :: Config -> FilePath -> FilePath -> Action Post
build config templName srcPath = cacheAction ("build" :: T.Text, srcFilePath) $ do
  liftIO . putStrLn $ "Rebuilding post: " <> srcFilePath
  postContent <- readFile' srcFilePath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTMLWithOpts pandocReaderOptions defaultHtml5Options . T.pack $ postContent
  let postUrl = T.pack $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = withSiteMeta siteMeta . withPostUrl $ postData
  templ <- compileTemplate' templPath
  writeFile' (outDir </> T.unpack postUrl) . T.unpack $ substitute templ fullPostData
  convert fullPostData
 where
  srcFilePath = srcDir config </> srcPath
  siteMeta = siteConfigToSiteMeta $ site config
  templPath = templatesDir config </> templName
  outDir = outputDir config

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Config -> [FilePattern] -> Action ()
copyStaticFiles config patts = do
  filepaths <- getDirectoryFiles baseDir patts
  void $ forP filepaths $ \filepath ->
    copyFileChanged (baseDir </> filepath) (outDir </> filepath)
 where
  baseDir = srcDir config
  outDir = outputDir config

formatDate :: String -> String
formatDate humanDate = toIsoDate parsedTime
 where
  parsedTime =
    parseTimeOrError True defaultTimeLocale "%b %e, %Y" humanDate :: UTCTime

rfc3339 :: Maybe String
rfc3339 = Just "%H:%M:%SZ"

toIsoDate :: UTCTime -> String
toIsoDate = formatTime defaultTimeLocale (iso8601DateFormat rfc3339)

dropDirectoryN :: Int -> FilePath -> FilePath
dropDirectoryN 0 fp = fp
dropDirectoryN n fp = dropDirectoryN (n - 1) (dropDirectory1 fp)
dropDirectory :: FilePath -> FilePath -> FilePath
dropDirectory src = dropDirectoryN (length $ splitPath $ normaliseEx src)

buildRule :: Config -> Value -> Rule -> Action Value
buildRule config v (Rule{action = CopyRule{}, patterns = p}) = do
  copyStaticFiles config p
  return v
buildRule config (Object v) (Rule{name = n, action = BuildRule{template = t}, patterns = p}) = do
  ps <- builds config p t
  case n of
    Just na -> return (toJSON $ union (singleton (fromString na) $ toJSON ps) v)
    Nothing -> return (Object v)
buildRule _ _ _ = error ""

buildRules :: Config -> Value -> [Rule] -> Action Value
buildRules config val (r : rs) = do
  v <- buildRule config val r
  buildRules config v rs
buildRules _ val [] = do
  return val

buildSite :: Config -> Action ()
buildSite config = do
  _ <- buildRules config (object []) rls
  return ()
 where
  rls = rules config

main :: IO ()
main = do
  configEither <- decodeFileEither configFile
  case configEither of
    Left _ -> do
      hPrint stderr (configFile ++ " read error")
      exitWith (ExitFailure 1)
    Right config -> do
      let shOpts = shakeOptions{shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
      shakeArgsForward shOpts $ buildSite config
