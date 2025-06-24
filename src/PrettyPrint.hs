{-# LANGUAGE OverloadedStrings #-}

module PrettyPrint (
    prettyPrintPlan,
    prettyPrintTimedLifeChunk,
    -- Potentially re-export or enhance summary formatting if needed
    formatDaySummary,
    formatCategorySummary,
    formatDuration -- Make sure this is the one we want to use consistently
) where

import Types
import Parse (LifeLapse(..), TimedLifeChunk(..), LifeChunk(..), Timestamp) -- Assuming necessary types are exported
import Summary (DaySummary, CategorySummary(..), formatDaySummary, formatCategorySummary, formatDuration) -- Import existing summary formatters

import qualified Data.Text as T
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time (ZonedTime, Day)
import qualified Text.Printf as Printf (printf) -- Already used in Summary, ensure consistency

-- | Formats a complete plan from a list of LifeLapses.
-- A plan typically spans multiple days, each starting with a LifeLapse.
prettyPrintPlan :: [LifeLapse] -> [String]
prettyPrintPlan [] = ["No plan entries."]
prettyPrintPlan lapses = concatMap formatSingleLapseAsPlanEntry lapses

-- | Formats a single LifeLapse as part of a plan.
-- This includes the start date of the lapse and all its timed chunks.
formatSingleLapseAsPlanEntry :: LifeLapse -> [String]
formatSingleLapseAsPlanEntry lapse =
    let startDateStr = formatTime defaultTimeLocale "%Y-%m-%d" (llStart lapse)
        header = [Printf.printf "Plan for %s:" startDateStr, ""] -- Add a blank line after header
        chunkLines = map (prettyPrintTimedLifeChunk "  ") (llTokens lapse) -- Indent chunks
    in header ++ chunkLines ++ [""] -- Add a blank line after each day's plan

-- | Formats a single TimedLifeChunk with an optional prefix (e.g., for indentation).
prettyPrintTimedLifeChunk :: String -> TimedLifeChunk -> String
prettyPrintTimedLifeChunk prefix tlc =
    let startTimeStr = formatTime defaultTimeLocale "%H:%M" (tlcStart tlc)
        chunk = tlcLifeChunk tlc
        durationStr = formatDuration (lcDuration chunk)
        descriptionStr = T.unpack (lcDescription chunk) -- Convert Text to String
        categoriesStr = T.unpack $ T.intercalate " " (lcCategories chunk) -- Convert [Text] to String
        quadrantStr = show (lcQuadrant chunk) -- Q1, Q2 etc.

        -- Example output: "  08:00 (01h30m) [Q1] Meeting with team @projectX @client"
        -- Adjust formatting as per desired output style from Rust's pretty_print.
        -- Rust's format: "HH:MM (Dur) Quadrant Description Categories"
        -- Example: "09:00 (01h00) @1 Some activity @work"
        -- The Rust version seems to put quadrant before description.

        -- Let's try to match: StartTime (Duration) Quadrant Description Categories
        -- If description is empty, it might be omitted or handled.
        -- If categories are empty, they are omitted.

        descAndCats = T.strip $ T.pack $ descriptionStr ++ if null categoriesStr then "" else " " ++ categoriesStr

    in Printf.printf "%s%s (%s) %s %s"
        prefix
        startTimeStr
        durationStr
        quadrantStr
        (T.unpack descAndCats)


-- Note on formatDuration:
-- The `formatDuration` function is currently defined in `Summary.hs`.
-- For consistency, it should ideally be in a shared `Utils.hs` or passed as a dependency.
-- If it's re-defined here, ensure it's identical or make a clear choice.
-- For now, we assume it's imported from Summary.hs and is the canonical version.

-- Placeholder for writing to files or stdout - this will be in Main.hs usually
-- writeFileLines :: FilePath -> [String] -> IO ()
-- writeFileLines path lines = writeFile path (unlines lines)

-- printLines :: [String] -> IO ()
-- printLines = mapM_ putStrLn

```
