{-# LANGUAGE OverloadedStrings #-}

module Summary (
    computeDaySummary,
    computeAllSummaries,
    mergeSummariesOnSameDate,
    formatDaySummary,
    formatCategorySummary, -- For global summary
    computeGlobalCategorySummary,
    DaySummary,
    CategorySummary(..),
    Timestamp,
    LifeLapse(..),
    TimedLifeChunk(..),
    LifeChunk(..),
    Category,
    Duration(..)
) where

import Types
import Parse (LifeLapse(..), TimedLifeChunk(..), LifeChunk(..), Timestamp, Category, Duration(..)) -- Assuming these are exported

import qualified Data.Map.Strict as Map
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Time (ZonedTime, Day, localDay, zonedTimeToLocalTime)
import qualified Text.Printf as Printf -- For duration formatting

-- Computes a summary for a single list of TimedLifeChunks (typically one LifeLapse)
computeDaySummary :: [TimedLifeChunk] -> DaySummary
computeDaySummary chunks =
    foldl' addObjective Map.empty chunks
  where
    addObjective :: DaySummary -> TimedLifeChunk -> DaySummary
    addObjective currentSummary tlc =
        let chunk = tlcLifeChunk tlc
            cats = lcCategories chunk
            dur = lcDuration chunk
        in foldl' (\summary cat -> Map.insertWith addDur cat dur summary) currentSummary cats

    addDur :: Duration -> Duration -> Duration
    addDur (Duration d1) (Duration d2) = Duration (d1 + d2)

-- Computes summaries for all LifeLapses
computeAllSummaries :: [LifeLapse] -> [(Timestamp, DaySummary)]
computeAllSummaries lapses =
    map (\lapse -> (llStart lapse, computeDaySummary (llTokens lapse))) lapses

-- Merges summaries that occur on the same calendar date.
-- Assumes the input list is sorted by Timestamp, which parseActivityFiles should ensure.
mergeSummariesOnSameDate :: [(Timestamp, DaySummary)] -> [(Day, DaySummary)]
mergeSummariesOnSameDate timedSummaries =
    let
        -- Convert Timestamp to Day for grouping
        dayTaggedSummaries :: [(Day, DaySummary)]
        dayTaggedSummaries = map (\(ts, summary) -> (zonedTimeToDay ts, summary)) timedSummaries

        -- Group by Day
        groupedByDay :: [[(Day, DaySummary)]]
        groupedByDay = groupBy ((==) `on` fst) $ sortBy (compare `on` fst) dayTaggedSummaries
        -- The sortBy might be redundant if input is already sorted, but ensures correctness.

        -- Merge summaries within each group
        mergedSummaries :: [(Day, DaySummary)]
        mergedSummaries = map mergeGroup groupedByDay
          where
            mergeGroup :: [(Day, DaySummary)] -> (Day, DaySummary)
            mergeGroup group =
                let day = fst (head group)
                    summariesToMerge = map snd group
                    merged = foldl' (Map.unionWith addDur) Map.empty summariesToMerge
                in (day, merged)

            addDur (Duration d1) (Duration d2) = Duration (d1 + d2)
    in
        mergedSummaries

zonedTimeToDay :: ZonedTime -> Day
zonedTimeToDay = localDay . zonedTimeToLocalTime

-- Formats a single day's summary into a list of strings
formatDaySummary :: (Day, DaySummary) -> [String]
formatDaySummary (day, summaryMap)
    | Map.null summaryMap = []
    | otherwise =
        let header = Printf.printf "%s (summary)" (show day) -- TODO: Nicer date formatting
            categoryLines = map formatCatDurPair (Map.toAscList summaryMap)
        in header : categoryLines ++ [""] -- Add a blank line at the end
  where
    formatCatDurPair :: (Category, Duration) -> String
    formatCatDurPair (cat, dur) =
        Printf.printf "%s: %s" (show cat) (formatDuration dur) -- Using show for Text for now

-- Formats a duration (e.g., 90 minutes -> "01h30m")
-- This is duplicated from Types.hs (where it was commented out).
-- It should live in a common Util module or be passed around.
formatDuration :: Duration -> String
formatDuration (Duration totalMinutes) =
    let hours = totalMinutes `div` 60
        minutes = totalMinutes `mod` 60
    in Printf.printf "%02dh%02dm" hours minutes

-- For the global summary:
-- 1. Aggregate all DaySummary maps into one.
-- 2. Convert this aggregated map into a list of CategorySummary.

-- Computes a global summary from a list of DaySummaries
computeGlobalSummaryMap :: [(Day, DaySummary)] -> DaySummary
computeGlobalSummaryMap daySummaries =
    foldl' (Map.unionWith addDur) Map.empty (map snd daySummaries)
  where
    addDur (Duration d1) (Duration d2) = Duration (d1 + d2)

-- Converts the global DaySummary map to a list of CategorySummary, sorted by category name
computeGlobalCategorySummary :: [(Day, DaySummary)] -> [CategorySummary]
computeGlobalCategorySummary daySummaries =
    let globalSummaryMap = computeGlobalSummaryMap daySummaries
        categorySummaryList = map (\(cat, dur) -> CategorySummary cat dur) (Map.toAscList globalSummaryMap)
    in categorySummaryList -- Already sorted by category name due to Map.toAscList

-- Formats a list of CategorySummary (typically for the global summary)
formatCategorySummary :: String -> [CategorySummary] -> [String]
formatCategorySummary title categorySummaries
    | null categorySummaries = []
    | otherwise =
        let header = title
            categoryLines = map formatCsPair categorySummaries
        in header : categoryLines ++ [""]
  where
    formatCsPair :: CategorySummary -> String
    formatCsPair (CategorySummary name dur) =
        Printf.printf "%s: %s" (show name) (formatDuration dur) -- Using show for Text for now

```
