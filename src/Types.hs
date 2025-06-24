{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Int (Int64) -- For Duration, which is often represented as nanoseconds or similar
import GHC.Generics (Generic)
import Data.Text (Text) -- Using Text for strings is common in Haskell

-- From config.rs

type Category = Text

data Quadrant
    = Q1
    | Q2
    | Q3
    | Q4
    | Q5
    | Q6 -- Q6 seems to be a default or "none"
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

-- Using Int64 for duration, typically representing nanoseconds or picoseconds
-- The 'time' library's NominalDiffTime or DiffTime could also be used.
-- For simplicity, starting with Int64 representing minutes as in the Rust code's `Duration::hours` and `Duration::minutes`.
-- Let's define it as minutes for now.
newtype Duration = Duration { getDuration :: Int64 }
    deriving (Show, Eq, Ord, Num, Generic)

data Config = Config
    { cfgQuadrants :: Map.Map Quadrant [Category]
    , cfgNotify :: Bool
    , cfgQuiet :: Bool
    , cfgWatch :: Bool
    , cfgSummaryOut :: Maybe FilePath
    , cfgPlanOut :: Maybe FilePath
    , cfgActivityPaths :: Set.Set FilePath
    -- cfgGlobalConfigPath is not explicitly in Rust's Config but implied by how it's loaded.
    -- For now, focusing on direct translations.
    } deriving (Show, Eq, Generic)

defaultConfig :: Config
defaultConfig = Config
    { cfgQuadrants = Map.empty
    , cfgNotify = False
    , cfgQuiet = False
    , cfgWatch = False
    , cfgSummaryOut = Nothing
    , cfgPlanOut = Nothing
    , cfgActivityPaths = Set.empty
    }

-- From parse.rs

-- Equivalent to chrono::DateTime<Local>
type Timestamp = ZonedTime -- Or LocalTime if timezone is handled consistently elsewhere

data LifeChunk = LifeChunk
    { lcDescription :: Text
    , lcDuration :: Duration -- Using our defined Duration type (minutes)
    , lcCategories :: [Category]
    , lcQuadrant :: Quadrant
    , lcUserProvidedQuadrant :: Bool
    , lcInput :: Text -- The original input string for this chunk
    } deriving (Show, Eq, Generic)

data TimedLifeChunk = TimedLifeChunk
    { tlcStart :: Timestamp
    , tlcLifeChunk :: LifeChunk
    } deriving (Show, Eq, Generic)

-- Represents a continuous sequence of activities for a single day or period
data LifeLapse = LifeLapse
    { llStart :: Timestamp
    , llEnd :: Timestamp -- Calculated as start + total duration of tokens
    , llTokens :: [TimedLifeChunk]
    } deriving (Show, Eq, Generic)

-- Represents results from parsing a line
data LineParseResult
    = Lc { lprLifeChunk :: LifeChunk }
    | DateResult { lprDate :: Timestamp }
    -- Rust also had a ParseError variant, which can be handled via Either or Maybe in Haskell
    deriving (Show, Eq, Generic)

-- Represents either a piece of an activity or a new date marker during parsing
data TiroToken
    = TiroTlc { ttTlc :: TimedLifeChunk }
    | TiroDate { ttDate :: Timestamp }
    deriving (Show, Eq, Generic)


-- From summary.rs

-- Summary mapping category name to total duration
type DaySummary = Map.Map Category Duration

-- For displaying summaries, associating a name (category) with its duration
data CategorySummary = CategorySummary
    { csName :: Category
    , csDuration :: Duration
    } deriving (Show, Eq, Ord, Generic)


-- Error type (simple version)
newtype TiroError = TiroError { unTiroError :: String }
    deriving (Show, Eq)

type TiroResult a = Either TiroError a

-- From parse_state.rs (mentioned in config.rs and parse.rs)
-- This state seems to be used during parsing to map categories found in files
-- to their respective quadrants, potentially updated by global config.
data ParseState = ParseState
    { psCategoriesToQuadrant :: Map.Map Category Quadrant
    -- Other fields if discovered from parse_state.rs, e.g., psKnownCategories: Set Category
    } deriving (Show, Eq, Generic)

emptyParseState :: ParseState
emptyParseState = ParseState { psCategoriesToQuadrant = Map.empty }

-- Helper for duration formatting, if needed directly in Types or a Util module later
formatDuration :: Duration -> String
formatDuration (Duration totalMinutes) =
    let hours = totalMinutes `div` 60
        minutes = totalMinutes `mod` 60
    in Printf.printf "%02dh%02dm" hours minutes

-- For formatDuration
-- Needs: import qualified Text.Printf as Printf (needed for formatDuration)
-- This will be added when we create the module that uses it.
-- For now, to make this file self-contained if compiled alone,
-- we can import it here or comment out formatDuration.
-- Let's assume it will be used elsewhere and keep the import in mind.
-- For now, I will comment out Printf and the function to avoid a dependency error if this file is compiled alone.
-- import qualified Text.Printf as Printf
-- formatDuration :: Duration -> String
-- formatDuration (Duration totalMinutes) =
-- let hours = totalMinutes `div` 60
-- minutes = totalMinutes `mod` 60
-- in Printf.printf "%02dh%02dm" hours minutes

-- Placeholder for MetaCategory if needed, from config.rs
-- It seems to distinguish between regular categories and explicit quadrant assignments in text.
data MetaCategory = MetaRegularCategory Text (Maybe Quadrant) | MetaQuad Quadrant
    deriving (Show, Eq, Generic)

-- End of Types.hs
-- Note: The Rust `time::Duration` is more fine-grained (nanoseconds).
-- My `Duration` is currently minutes. This might need adjustment if precision becomes an issue.
-- The Rust code uses `chrono::Duration` (via `time::Duration` re-export) for `LifeChunk`
-- and `std::time::Duration` for watcher delays.
-- `chrono::Duration` in Rust is capable of representing days, hours, minutes, seconds, millis, micros, nanos.
-- The parsing logic `Duration::hours(h)` and `Duration::minutes(m)` suggests that the input format is hours and minutes.
-- So, storing as total minutes in `Types.Duration` seems reasonable for `LifeChunk`.
-- `Timestamp` is `DateTime<Local>` in Rust, which is `ZonedTime` in Haskell's `Data.Time`.
-- `Date<Local>` in Rust (for summaries) is `Day` in Haskell.
```
