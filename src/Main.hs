module Main where

import Types
import Config (loadConfig, cfgPlanOut, cfgSummaryOut, cfgQuiet)
import Parse (parseActivityFiles)
import Summary (computeAllSummaries, mergeSummariesOnSameDate, computeGlobalCategorySummary, formatDaySummary, formatCategorySummary)
import PrettyPrint (prettyPrintPlan) -- Using this for plan output

import Control.Monad (when, unless)
import qualified Data.Text.IO as TIO
import System.IO (stdout, stderr, hPutStrLn)
import qualified Data.Set as Set

-- Helper to write lines to a file or stdout
writeOutput :: Maybe FilePath -> [String] -> IO ()
writeOutput Nothing linesToWrite = mapM_ putStrLn linesToWrite
writeOutput (Just filePath) linesToWrite = writeFile filePath (unlines linesToWrite)

main :: IO ()
main = do
    -- 1. Load Configuration
    configResult <- loadConfig
    case configResult of
        Left (TiroError err) -> hPutStrLn stderr $ "Error loading configuration: " ++ err
        Right config -> do
            unless (cfgQuiet config) $ do
                putStrLn "Configuration loaded."
                -- print config -- For debugging config loading

            -- 2. Parse Activities
            -- Make sure cfgActivityPaths is being used correctly by parseActivityFiles
            unless (cfgQuiet config) $
                putStrLn $ "Parsing activity files: " ++ show (Set.toList $ cfgActivityPaths config)

            parseResult <- parseActivityFiles config
            case parseResult of
                Left (TiroError err) -> hPutStrLn stderr $ "Error parsing activity files: " ++ err
                Right (startTime, lifeLapses) -> do
                    unless (cfgQuiet config) $ putStrLn "Activity files parsed successfully."

                    -- 3. Compute Summaries
                    let timedSummaries = computeAllSummaries lifeLapses
                    let dailySummaries = mergeSummariesOnSameDate timedSummaries
                    let globalCatSummaries = computeGlobalCategorySummary dailySummaries

                    unless (cfgQuiet config) $ putStrLn "Summaries computed."

                    -- 4. Format and 5. Write Plan Output
                    let planOutputLines = prettyPrintPlan lifeLapses
                    unless (cfgQuiet config) $
                        case cfgPlanOut config of
                            Just path -> putStrLn $ "Writing plan to " ++ path
                            Nothing   -> putStrLn "Writing plan to stdout."
                    writeOutput (cfgPlanOut config) planOutputLines

                    -- 4. Format and 5. Write Summary Output (Daily and Global)
                    let dailySummaryOutputLines = concatMap formatDaySummary dailySummaries
                    let globalSummaryTitle = "Global Summary:"
                    let globalSummaryOutputLines = formatCategorySummary globalSummaryTitle globalCatSummaries

                    let summaryOutputLines = dailySummaryOutputLines ++ [""] ++ globalSummaryOutputLines

                    unless (cfgQuiet config) $
                        case cfgSummaryOut config of
                            Just path -> putStrLn $ "Writing summaries to " ++ path
                            Nothing   -> putStrLn "Writing summaries to stdout."
                    writeOutput (cfgSummaryOut config) summaryOutputLines

                    unless (cfgQuiet config) $ putStrLn "Processing complete."

-- Note: Watch mode (`cfgWatch config`) is not implemented in this main loop yet.
-- That would require a different structure, likely involving fsnotify and a recurring loop.
```
