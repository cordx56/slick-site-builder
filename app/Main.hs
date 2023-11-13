{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens (at, (?~))
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), Key, ToJSON, Value (Object, String, Array), object, toJSON, (.:), (.:?))
import Data.Aeson.Key (fromString)
import Data.Aeson.KeyMap as KeyMap (KeyMap, empty, lookup, singleton, toList, union)
import Data.Aeson.Lens (_Object)
import Data.List (sort)
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
import Language.Haskell.Interpreter (interpret, runInterpreter, setImports)
import qualified Language.Haskell.Interpreter as Hint
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
  = CopyRule {}
  | BuildRule
      { _brTemplate :: FilePath
      , _brLet :: Maybe [KeyMap String]
      }

instance FromJSON RuleAction where
  parseJSON (Object v) = case KeyMap.lookup "name" v of
    (Just "build") ->
      BuildRule
        <$> v
        .: "template"
        <*> v
        .:? "let"
    _ -> error "unknown rule action"
  parseJSON (String "copy") = pure CopyRule{}
  parseJSON _ = error "action must be object"

data BuildState = BuildState
  { values :: Value
  , built :: [FilePath]
  }

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

unionValues :: Value -> Value -> Value
unionValues (Object obj1) (Object obj2) = Object $ union obj1 obj2
unionValues _ _ = error "type error"
unionAllValues :: [Value] -> Value
unionAllValues = foldr unionValues (Object empty)

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
builds :: Config -> BuildState -> FilePath -> [FilePath] -> Action [Value]
builds config state templName pPaths = do
  forP pPaths $ build config state templName

type TemplatePath = FilePath

{- | Load a post, process metadata, write it to output, then return the post object
Detects changes to either post content or template
-}
build :: Config -> BuildState -> TemplatePath -> FilePath -> Action Value
-- removed cacheAction for returning Value -- Maybe I should use cacheAction
build config (BuildState{values = val}) templName srcPath = do
  liftIO . putStrLn $ "Rebuilding post: " <> srcFilePath
  postContent <- readFile' srcFilePath
  -- load post content and metadata as JSON blob
  postData <- markdownToHTMLWithOpts pandocReaderOptions defaultHtml5Options . T.pack $ postContent
  let postUrl = T.pack $ srcPath -<.> "html"
      withPostUrl = _Object . at "url" ?~ String postUrl
  -- Add additional metadata we've been able to compute
  let fullPostData = unionValues val $ withSiteMeta siteMeta . withPostUrl $ postData
  templ <- compileTemplate' templPath
  writeFile' (outDir </> T.unpack postUrl) . T.unpack $ substitute templ fullPostData
  convert fullPostData
 where
  srcFilePath = srcDir config </> srcPath
  siteMeta = siteConfigToSiteMeta $ site config
  templPath = templatesDir config </> templName
  outDir = outputDir config

-- | Copy all static files from the listed folders to their destination
copyStaticFiles :: Config -> [FilePath] -> Action ()
copyStaticFiles config filepaths = do
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

getSrcFilesIncludingBuilt :: Config -> [FilePattern] -> Action [FilePath]
getSrcFilesIncludingBuilt config = do
  getDirectoryFiles baseDir
 where
  baseDir = srcDir config
getSrcFiles :: Config -> BuildState -> [FilePattern] -> Action [FilePath]
getSrcFiles config (BuildState{built = bt}) patts = do
  includingBuilt <- getSrcFilesIncludingBuilt config patts
  return $ filter (`notElem` bt) includingBuilt

-- Evaluation system
sortValueBy :: Key -> Value -> Value
sortValueBy k (Array a) = do
commandInterpreter :: Value -> String -> Action Value
commandInterpreter v s =
  v
 where
  splitted = words s
eval :: Value -> String -> IO Value
eval v s = do
  r <- runInterpreter $ do
    setImports ["Prelude", "Data.List"]
    f <- interpret s (Hint.as :: Value -> Value)
    return $ f v
  case r of
    Left _ -> error "Interpretation Error"
    Right nv -> return nv
evalAndLet :: Value -> Key -> String -> IO Value
evalAndLet v k s = do
  r <- eval v s
  return $ unionValues v $ Object $ singleton k r
evalAll :: Value -> [(Key, String)] -> IO Value
evalAll v ((k, s) : xs) = do
  r <- evalAndLet v k s
  evalAll (unionValues v r) xs
evalAll v [] = do
  return v
evalLets :: Value -> [KeyMap String] -> IO Value
evalLets v (x : xs) = do
  let kvs = toList x
  vs <- evalAll v kvs
  evalLets vs xs
evalLets v [] = do
  return v

buildRule :: Config -> BuildState -> Rule -> Action BuildState
-- copy rule
buildRule config (BuildState{values = v, built = bt}) (Rule{action = CopyRule{}, patterns = p}) = do
  filepaths <- getSrcFiles config state p
  copyStaticFiles config filepaths
  return BuildState{values = v, built = bt ++ filepaths}
 where
  state = BuildState{values = v, built = bt}
-- build rule
buildRule config (BuildState{values = Object v, built = bt}) (Rule{name = nm, action = BuildRule{_brTemplate = t, _brLet = l}, patterns = p}) = do
  nv <- case l of
    Just ls -> do liftIO $ evalLets (Object v) ls
    Nothing -> do return $ Object v
  let state = BuildState{values = nv, built = bt}
  -- let nv = case n
  filepaths <- getSrcFiles config state p
  let sortedFilepaths = sort filepaths
  psts <- builds config state t sortedFilepaths
  let newBt = bt ++ filepaths
  case nm of
    Just nam -> do
      let newVals = toJSON $ union (singleton (fromString nam) $ toJSON psts) v
      return $ BuildState{values = newVals, built = newBt}
    Nothing -> return $ BuildState{values = Object v, built = newBt}
buildRule _ _ _ = error ""

buildRules :: Config -> BuildState -> [Rule] -> Action BuildState
buildRules config state (r : rs) = do
  s <- buildRule config state r
  buildRules config s rs
buildRules _ state [] = do
  return state

buildSite :: Config -> Action ()
buildSite config = do
  _ <- buildRules config (BuildState{values = object [], built = []}) rls
  return ()
 where
  rls = rules config

main :: IO ()
main = do
  configEither <- decodeFileEither configFile
  case configEither of
    Left e -> do
      hPrint stderr (configFile ++ " read error")
      hPrint stderr e
      exitWith (ExitFailure 1)
    Right config -> do
      let shOpts = shakeOptions{shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
      shakeArgsForward shOpts $ buildSite config
