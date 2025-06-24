{-# LANGUAGE OverloadedStrings #-}

module Parse (
    parseActivityFiles,
    processLines,
    parseLine,
    getLifeChunk,
    getParseStateFromChunks,
    applyParseStateToChunks,
    updateParseStateFromConfig,
    parseDateTime,
    LifeChunk(..), -- Re-exporting for convenience if not importing Types directly
    LineParseResult(..),
    TimedLifeChunk(..),
    LifeLapse(..),
    TiroToken(..),
    ParseState(..), -- Re-exporting
    Timestamp,
    Config(..),
    Quadrant(..),
    Duration(..)
) where

import Types
import Config (Config(..)) -- Importing Config for updateParseStateFromConfig

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)
import Data.Time.LocalTime (LocalTime, ZonedTime, utcToZonedTime, zonedTimeToUTC, localTimeToUTC, TimeZone, utc, zonedTimeZone, timeToTimeOfDay, NominalDiffTime, DiffTime, getCurrentTimeZone, zonedTimeToLocalTime)
import Data.Time.Clock (UTCTime(..), addUTCTime, secondsToNominalDiffTime, picosecondsToDiffTime)
import Data.Time.Calendar (Day(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl', sortOn)
import Data.Maybe (listToMaybe, fromMaybe, isJust, mapMaybe)
import Control.Monad (msum)
import Text.Read (readMaybe)
import Data.Char (isSpace, isDigit)
import Data.Either (partitionEithers)


-- Replicates parts of Rust's parse_state.rs
getParseStateFromChunks :: [LineParseResult] -> ParseState
getParseStateFromChunks lprs =
    foldl' accumulateCategories emptyParseState lprs
  where
    accumulateCategories :: ParseState -> LineParseResult -> ParseState
    accumulateCategories ps (Lc chunk) =
        -- Only register if the chunk's quadrant is not the default one,
        -- implying it was likely set by a user tag (e.g. @Q1) or already resolved.
        -- The Rust code checks `chunk.quadrant == Default::default()`, which is Q6.
        -- If `lcUserProvidedQuadrant` is true, it means a quadrant like @1 was in the line.
        if lcUserProvidedQuadrant chunk && lcQuadrant chunk /= Q6 then
            let currentMap = psCategoriesToQuadrant ps
                updatedMap = foldl' (\m cat -> Map.insert cat (lcQuadrant chunk) m) currentMap (lcCategories chunk)
            in ps { psCategoriesToQuadrant = updatedMap }
        else
            ps
    accumulateCategories ps _ = ps


applyParseStateToChunks :: ParseState -> [LineParseResult] -> [LineParseResult]
applyParseStateToChunks ps = map updateLprQuadrant
  where
    updateLprQuadrant :: LineParseResult -> LineParseResult
    updateLprQuadrant (Lc chunk) =
        if not (lcUserProvidedQuadrant chunk) then
            case lookupQuadrantForChunk ps chunk of
                Just newQuadrant -> Lc (chunk { lcQuadrant = newQuadrant })
                Nothing -> Lc chunk
        else
            Lc chunk
    updateLprQuadrant dateRes = dateRes

    lookupQuadrantForChunk :: ParseState -> LifeChunk -> Maybe Quadrant
    lookupQuadrantForChunk parseState chunk =
        listToMaybe $ mapMaybe (`Map.lookup` psCategoriesToQuadrant parseState) (lcCategories chunk)

-- Updates ParseState based on global config (config.rs -> update_parse_state_from_config)
updateParseStateFromConfig :: Config -> ParseState -> ParseState
updateParseStateFromConfig config ps =
    let configQuads = cfgQuadrants config
        currentCatToQuad = psCategoriesToQuadrant ps

        -- Fold over the config's quadrant assignments
        -- Add to parse state only if category not already there
        -- Config entries do not override categories learned from file tags
        newCatToQuad = Map.foldrWithKey folder currentCatToQuad configQuads
          where
            folder :: Quadrant -> [Category] -> Map.Map Category Quadrant -> Map.Map Category Quadrant
            folder quad cats accMap =
                foldl' (\m cat -> Map.insertWith (\_ existingVal -> existingVal) cat quad m) accMap cats
                -- Map.insertWith makes sure not to override existing if key is present
                -- The Rust code: `if !parse_state.categories_to_quadrant.contains_key(s)`
                -- So, if it exists, we keep existing. `Map.union` with preference for left map (psCategoriesToQuadrant) achieves this.
                -- Let's refine: iterate config and only insert if not present in ps.

        -- Correct logic: Iterate through config quadrants. For each category, if it's NOT in ps, add it.
        finalMap = Map.foldlWithKey' (\accMap quad catList ->
            foldl' (\innerMap cat ->
                Map.alter (\maybeVal -> case maybeVal of
                    Nothing -> Just quad -- Not in ps, add from config
                    Just existingQuad -> Just existingQuad -- Already in ps, keep it
                ) cat innerMap
            ) accMap catList
          ) currentCatToQuad configQuads

    in ps { psCategoriesToQuadrant = finalMap }


-- Main parsing function for activity files
parseActivityFiles :: Config -> IO (TiroResult (Timestamp, [LifeLapse]))
parseActivityFiles config = do
    -- For now, handling only the first activity file if multiple are provided via CLI or TOML
    -- The Rust version reads all of them and concatenates lines.
    -- We should adapt to merge lines from all specified files.
    let filePaths = Set.toList $ cfgActivityPaths config

    allFileLines <- mapM readLinesFromFileOrStdin filePaths
    case partitionEithers allFileLines of
        (errors, linesLists) | not (null errors) -> return $ Left (head errors) -- Return first error
        (_, linesLists) -> do
            let combinedLines = concat linesLists
            if null combinedLines
            then return $ Left $ TiroError "No lines found in any activity file."
            else do
                -- Get current TimeZone for parsing date/time strings accurately
                localTZ <- getCurrentTimeZone
                let initialParseResults = map (parseLine localTZ) combinedLines

                -- 1. Initial pass to register categories with explicitly set quadrants from file
                let fileParseState = getParseStateFromChunks initialParseResults
                -- 2. Update this parse state with global config (config doesn't override file specifics)
                let configAppliedParseState = updateParseStateFromConfig config fileParseState
                -- 3. Apply the final parse state to update chunks that didn't have a quadrant
                let finalParseResults = applyParseStateToChunks configAppliedParseState initialParseResults

                case processLines finalParseResults of
                    Left err -> return $ Left err
                    Right (startTime, lapses) -> return $ Right (startTime, lapses)


readLinesFromFileOrStdin :: FilePath -> IO (TiroResult [T.Text])
readLinesFromFileOrStdin "-" = Right . T.lines <$> TIO.getContents -- Read from stdin
readLinesFromFileOrStdin filePath = do
    exists <- doesFileExist filePath
    if not exists
        then return $ Left $ TiroError $ "Activity file not found: " ++ filePath
        else Right . T.lines <$> TIO.readFile filePath


-- Parses a single line from an activity file, given the local TimeZone
parseLine :: TimeZone -> T.Text -> LineParseResult
parseLine tz line =
    if T.null trimmedLine || T.isPrefixOf "#" trimmedLine -- Skip empty lines and comments
    then Lc (emptyLifeChunk line) -- Or a more specific "Comment" or "Empty" type if needed
    else case parseDateTime tz (T.unpack trimmedLine) of
        Just ts -> DateResult ts
        Nothing -> Lc (getLifeChunk line)
  where
    trimmedLine = T.strip line
    emptyLifeChunk :: T.Text -> LifeChunk
    emptyLifeChunk t = LifeChunk "" (Duration 0) [] Q6 False t


-- Parses a date-time string. Rust uses Local.datetime_from_str with various formats.
-- Example formats: "%Y-%m-%d %H:%M", "%Y-%m-%d %Hh%M"
-- Haskell's parseTimeM is powerful. We need to provide the right TimeZone.
-- For simplicity, assuming UTC for parsing, then converting to local ZonedTime if needed.
-- The Rust code uses `Local.ymd(...).and_hms(...)` which implies local timezone interpretation.
-- `ZonedTime` is essential here.
-- This function is IO because getCurrentTimeZone is IO.
-- To avoid making the pure `parseLine` IO, we would need to pass TimeZone as an argument.
-- For now, making parseLine also IO, or refactoring to pass TimeZone.
-- Let's assume for the plan that `parseLine` will remain pure and `parseDateTime` will take TimeZone.
-- For this iterative step, I'll make parseLine IO to proceed quickly. This is a temporary change.
--
-- UPDATE: Decided to pass TimeZone to parseLine and parseDateTime to keep them pure where possible.
-- The IO action to get TimeZone will be in parseActivityFiles.

-- Parses a date-time string using a given TimeZone.
parseDateTime :: TimeZone -> String -> Maybe ZonedTime
parseDateTime tz str =
    let tryParse fmt = parseTimeM True defaultTimeLocale fmt str :: Maybe LocalTime
        formats = ["%Y-%m-%d %H:%M", "%Y-%m-%d %Hh%M", iso8601DateFormat (Just "%H:%M:%S")]
        mlocalTime = msum (map tryParse formats)
    in case mlocalTime of
        Just lt -> Just $ ZonedTime lt tz
        Nothing -> Nothing


-- Parses a line into a LifeChunk. Rust: get_life_chunk
getLifeChunk :: T.Text -> LifeChunk
getLifeChunk line =
    let parts = T.words line
        (durationParts, remainingParts) = consumeDurationParts parts
        (descWords, categoryParts) = consumeCategoryParts remainingParts

        description = T.unwords descWords
        rawCategories = map T.unpack categoryParts

        parsedCategories = mapMaybe parseCategoryToken categoryParts

        userQuadrant = listToMaybe [q | (MetaQuad q) <- parsedCategories]
        regularCategories = [T.unpack txt | (MetaRegularCategory txt _) <- parsedCategories]

        finalQuadrant = fromMaybe Q6 userQuadrant

    in LifeChunk
        { lcDescription = description
        , lcDuration = parseDuration durationParts
        , lcCategories = regularCategories ++ (if isJust userQuadrant then [T.pack $ show $ fromJust userQuadrant] else []) -- Include quadrant tag like @Q1 as a category
        , lcQuadrant = finalQuadrant
        , lcUserProvidedQuadrant = isJust userQuadrant
        , lcInput = line -- Store the original line
        }
  where
    parseDuration :: [T.Text] -> Duration
    parseDuration [] = Duration 0
    parseDuration [h] = Duration (fromIntegral (readMaybe (T.unpack h) ?: 0) * 60)
    parseDuration (h:m:_) = Duration (fromIntegral ((readMaybe (T.unpack h) ?: 0) * 60 + (readMaybe (T.unpack m) ?: 0)))
    infixr 0 ?:
    (?:) :: Maybe a -> a -> a
    mb ?: d = fromMaybe d mb

    consumeDurationParts :: [T.Text] -> ([T.Text], [T.Text])
    consumeDurationParts [] = ([], [])
    consumeDurationParts (p1:ps) | T.all isDigit p1 =
        case ps of
            (p2:ps') | T.all isDigit p2 -> ([p1, p2], ps')
            _ -> ([p1], ps)
    consumeDurationParts ps = ([], ps)

    consumeCategoryParts :: [T.Text] -> ([T.Text], [T.Text])
    consumeCategoryParts ps = (reverse desc, reverse cats)
      where (desc, cats) = foldl' distribute ([],[]) ps
            distribute (d,c) token | T.isPrefixOf "@" token = (d, token:c)
                                   | otherwise              = (token:d, c)

    parseCategoryToken :: T.Text -> Maybe MetaCategory
    parseCategoryToken token =
      if T.isPrefixOf "@" token then
          case stringToQuadrant (T.tail token) of -- T.tail to remove "@"
              Right q -> Just (MetaQuad q)
              Left _  -> Just (MetaRegularCategory (T.unpack token) Nothing) -- Store as regular category if not a valid Q format
      else
          Nothing -- Not a category token if it doesn't start with @

    -- Helper from Config.hs (or should be in Types/Utils)
    stringToQuadrant :: T.Text -> Either String Quadrant
    stringToQuadrant s = case T.toUpper s of
        "Q1" -> Right Q1; "1" -> Right Q1
        "Q2" -> Right Q2; "2" -> Right Q2
        "Q3" -> Right Q3; "3" -> Right Q3
        "Q4" -> Right Q4; "4" -> Right Q4
        "Q5" -> Right Q5; "5" -> Right Q5
        "Q6" -> Right Q6; "6" -> Right Q6
        _    -> Left $ "Invalid quadrant string: " ++ T.unpack s


-- Processes parsed lines into LifeLapses. Rust: parse_activities & get_all_life_lapses
processLines :: [LineParseResult] -> TiroResult (Timestamp, [LifeLapse])
processLines [] = Left $ TiroError "No lines to process."
processLines (DateResult initialDate : rest) =
    let (tiroTokens, _) = foldl' processToken ([], initialDate) rest
        lapses = groupTokensIntoLapses initialDate tiroTokens
        -- Rust logic: find min start time, sort, filter, merge.
        -- Here, assuming chronological order from file. Merging might be needed.
    in if null lapses then Left (TiroError "No activities found after processing.")
       else Right (initialDate, lapses) -- TODO: Use actual earliest start time
processLines _ = Left $ TiroError "Activity file must start with a date line."

processToken :: ([TiroToken], Timestamp) -> LineParseResult -> ([TiroToken], Timestamp)
processToken (accTokens, currentTime) (Lc chunk) =
    let timedChunk = TimedLifeChunk currentTime chunk
        newTime = addDuration currentTime (lcDuration chunk)
    in (accTokens ++ [TiroTlc timedChunk], newTime)
processToken (accTokens, _) (DateResult newDate) =
    (accTokens ++ [TiroDate newDate], newDate)

-- Helper to add Duration (in minutes) to a Timestamp (ZonedTime)
addDuration :: ZonedTime -> Duration -> ZonedTime
addDuration zt (Duration minutes) =
    let utcTime = zonedTimeToUTC zt
        nominalDiffTime = fromIntegral minutes * 60 -- Convert minutes to NominalDiffTime (seconds)
        newUtcTime = addUTCTime nominalDiffTime utcTime
    in utcToZonedTime (zonedTimeZone zt) newUtcTime -- Convert back using original timezone

addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime diff (UTCTime day time) = UTCTime day (addAbsoluteTime diff time)

addAbsoluteTime :: NominalDiffTime -> DiffTime -> DiffTime
addAbsoluteTime diffTime timeOfDay =
    timeOfDayToTime timeOfDay + diffTime


-- Groups TiroTokens into LifeLapses
groupTokensIntoLapses :: Timestamp -> [TiroToken] -> [LifeLapse]
groupTokensIntoLapses _ [] = []
groupTokensIntoLapses initialStartTime tokens = go initialStartTime [] tokens
  where
    go :: Timestamp -> [TimedLifeChunk] -> [TiroToken] -> [LifeLapse]
    go currentStartTime currentChunks [] =
        if null currentChunks
        then []
        else [makeLapse currentStartTime currentChunks]
    go currentStartTime currentChunks (TiroTlc tlc : ts) =
        go currentStartTime (currentChunks ++ [tlc]) ts
    go currentStartTime currentChunks (TiroDate newDate : ts) =
        let currentLapse = if null currentChunks then [] else [makeLapse currentStartTime currentChunks]
        in currentLapse ++ go newDate [] ts

    makeLapse :: Timestamp -> [TimedLifeChunk] -> LifeLapse
    makeLapse start tlcList =
        let totalDur = sumDuration (map (lcDuration . tlcLifeChunk) tlcList)
            endTime = addDuration start totalDur
        in LifeLapse start endTime tlcList

    sumDuration :: [Duration] -> Duration
    sumDuration ds = Duration $ sum $ map getDuration ds
```
