{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (
    loadConfig,
    CliOptions(..),
    TomlConfig(..), -- Exporting for potential direct use or testing
    parseTomlConfig, -- Exporting for testing or direct use
    applyCliOptionsToConfig,
    processConfigLoading
) where

import Types
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.FilePath (takeDirectory)
import System.Directory (canonicalizePath, doesFileExist)
import Control.Monad (foldM, filterM, forM)
import GHC.Generics (Generic)
import qualified Toml
import Data.Either (rights)


-- Data structure to hold command-line options
data CliOptions = CliOptions
    { cliActivities :: [FilePath]
    , cliPlanOut :: Maybe FilePath
    , cliSummaryOut :: Maybe FilePath
    , cliWatch :: Bool
    , cliNotify :: Bool
    , cliQuiet :: Bool
    , cliConfigPath :: FilePath -- Required
    } deriving (Show, Eq)

-- Data structure for what we expect from the TOML file
-- Mirroring RawConfig from Rust's config.rs
data TomlConfig = TomlConfig
    { tcActivityPaths :: [FilePath]
    , tcQuadrants :: Map.Map T.Text [T.Text] -- Using Text as Quadrant keys initially
    } deriving (Show, Eq, Generic)

-- Parser for command-line options
cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions
    <$> many (strOption
        ( long "activities"
       <> short 'a'
       <> metavar "FILE"
       <> help "Activity file(s) to use. '-' for stdin." ))
    <*> optional (strOption
        ( long "plan"
       <> short 'p'
       <> metavar "FILE"
       <> help "Plan file to export to. Defaults to stdout." ))
    <*> optional (strOption
        ( long "summary"
       <> short 's'
       <> metavar "FILE"
       <> help "Summary file to export to. Defaults to stdout." ))
    <*> switch
        ( long "watch"
       <> short 'w'
       <> help "Watch the input file(s)." )
    <*> switch
        ( long "notify"
       <> short 'n'
       <> help "Notify when the next activity is close." )
    <*> switch
        ( long "quiet"
       <> short 'q'
       <> help "Quiet output." )
    <*> strOption
        ( long "config"
       <> short 'c'
       <> metavar "FILE"
       <> help "Custom config file (required)." )

-- Helper to extract a list of Text from a TOML Value (expecting an array of strings)
tableGetTexts :: Toml.Table -> T.Text -> Either String [T.Text]
tableGetTexts table key =
    case Toml.lookup key table of
        Just (Toml.Array arr) -> Right $ rights $ map Toml.string arr -- Filter out non-string values
        Just _ -> Left $ "Expected array of strings for key: " ++ T.unpack key
        Nothing -> Right [] -- Key not found, return empty list

-- Helper to extract a Table from a TOML Value (expecting a table)
tableGetTable :: Toml.Table -> T.Text -> Either String Toml.Table
tableGetTable table key =
    case Toml.lookup key table of
        Just (Toml.Table t) -> Right t
        Just _ -> Left $ "Expected a table for key: " ++ T.unpack key
        Nothing -> Right Map.empty -- Key not found, return empty table

-- Function to parse TOML content using toml-parser library
parseTomlContentReal :: T.Text -> Either String TomlConfig
parseTomlContentReal content =
    case Toml.parse content of
        Left err -> Left $ "TOML parse error: " ++ show err
        Right tomlTable -> do
            activityPathsText <- tableGetTexts tomlTable "activity_paths"
            let activityPaths = map T.unpack activityPathsText

            quadrantsTable <- tableGetTable tomlTable "quadrants"

            parsedQuadrants <- mapM (parseQuadrantEntry quadrantsTable) (Map.keys quadrantsTable)

            return $ TomlConfig activityPaths (Map.fromList parsedQuadrants)
          where
            parseQuadrantEntry :: Toml.Table -> T.Text -> Either String (T.Text, [T.Text])
            parseQuadrantEntry parentTable qKey = do
                val <- maybe (Left $ "Key " ++ T.unpack qKey ++ " not found in quadrants table") Right (Toml.lookup qKey parentTable)
                case val of
                    Toml.Array arr -> do
                        strValues <- forM arr $ \v -> case Toml.string v of
                                                          Right s -> Right s
                                                          Left _  -> Left $ "Non-string value found in quadrant array for " ++ T.unpack qKey
                        Right (qKey, strValues)
                    _ -> Left $ "Expected array for quadrant key: " ++ T.unpack qKey


-- Function to read and parse the TOML config file
parseTomlConfig :: FilePath -> IO (Either String TomlConfig)
parseTomlConfig cfgPath = do
    exists <- doesFileExist cfgPath
    if not exists
    then return $ Left $ "Config file not found: " ++ cfgPath
    else do
        content <- TIO.readFile cfgPath
        return $ parseTomlContentReal content -- Using the real parser

-- Convert string quadrant keys from TOML to Quadrant type
stringToQuadrant :: T.Text -> Either String Quadrant
stringToQuadrant s = case T.toUpper s of
    "Q1" -> Right Q1
    "1"  -> Right Q1
    "@1" -> Right Q1
    "Q2" -> Right Q2
    "2"  -> Right Q2
    "@2" -> Right Q2
    "Q3" -> Right Q3
    "3"  -> Right Q3
    "@3" -> Right Q3
    "Q4" -> Right Q4
    "4"  -> Right Q4
    "@4" -> Right Q4
    "Q5" -> Right Q5
    "5"  -> Right Q5
    "@5" -> Right Q5
    "Q6" -> Right Q6
    "6"  -> Right Q6
    "@6" -> Right Q6
    _    -> Left $ "Invalid quadrant string: " ++ T.unpack s

-- Convert TomlConfig to the main Config type
convertTomlToMainConfig :: TomlConfig -> FilePath -> IO (TiroResult Config)
convertTomlToMainConfig tc cfgFilePath = do
    let baseDir = takeDirectory cfgFilePath

    -- Resolve and filter activity paths
    resolvedPaths <- mapM (resolvePath baseDir) (tcActivityPaths tc)
    validPaths <- filterM doesFileExist resolvedPaths
    let activityPathSet = Set.fromList validPaths

    -- Convert quadrant map
    let convertedQuadrants = Map.fromListWith (++) <$> mapM convertQuadrantEntry (Map.toList $ tcQuadrants tc)

    case convertedQuadrants of
        Left err -> return $ Left $ TiroError err
        Right qMap -> return $ Right Config
            { cfgQuadrants = qMap
            , cfgNotify = False -- Defaults, will be overridden by CLI
            , cfgQuiet = False  -- Defaults
            , cfgWatch = False  -- Defaults
            , cfgSummaryOut = Nothing -- Defaults
            , cfgPlanOut = Nothing    -- Defaults
            , cfgActivityPaths = activityPathSet
            }
  where
    resolvePath base path = canonicalizePath (base ++ "/" ++ path) -- Simplified, needs robust path joining
    convertQuadrantEntry (qStr, cats) = do
        q <- stringToQuadrant qStr
        return (q, map T.unpack cats) -- Convert Text cats to String

-- Apply CLI options to override Config values
applyCliOptionsToConfig :: Config -> CliOptions -> IO Config
applyCliOptionsToConfig cfg opts = do
    cliActivityPaths <- Set.fromList <$> mapM canonicalizePath (cliActivities opts)
    return cfg
        { cfgNotify = cfgNotify cfg || cliNotify opts
        , cfgQuiet = cfgQuiet cfg || cliQuiet opts
        , cfgWatch = cfgWatch cfg || cliWatch opts
        , cfgSummaryOut = cliSummaryOut opts <|> cfgSummaryOut cfg -- CLI takes precedence
        , cfgPlanOut = cliPlanOut opts <|> cfgPlanOut cfg       -- CLI takes precedence
        , cfgActivityPaths = Set.union (cfgActivityPaths cfg) cliActivityPaths -- Merge paths
        }

-- Main function to load configuration
loadConfig :: IO (TiroResult Config)
loadConfig = do
    opts <- execParser $ info (cliOptionsParser <**> helper)
        ( fullDesc
       <> progDesc "Tiro - A command-line planning tool (Haskell version)"
       <> header "tiro-hs" )

    tomlEither <- parseTomlConfig (cliConfigPath opts)
    case tomlEither of
        Left err -> return $ Left $ TiroError err
        Right tomlCfg -> do
            initialConfigEither <- convertTomlToMainConfig tomlCfg (cliConfigPath opts)
            case initialConfigEither of
                Left err -> return $ Left err
                Right initialConfig -> Right <$> applyCliOptionsToConfig initialConfig opts

-- A simplified top-level processor for now
processConfigLoading :: IO ()
processConfigLoading = do
    configResult <- loadConfig
    case configResult of
        Left (TiroError err) -> putStrLn $ "Error loading configuration: " ++ err
        Right config -> do
            putStrLn "Configuration loaded successfully:"
            print config
            -- Further processing would happen here
            putStrLn $ "Effective activity paths: " ++ show (Set.toList $ cfgActivityPaths config)

-- Note: Environment variable substitution for paths like '$MOBILE_DIR'
-- as seen in Rust's `substitute_env_variable` is not yet implemented.
-- This would require additional logic, possibly using System.Environment.getEnv.
```
