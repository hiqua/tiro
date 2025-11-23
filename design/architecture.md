# Architecture Overview

`tiro` is designed as a pipeline that transforms raw activity logs into structured insights.

## High-Level Data Flow

1.  **Input**:
    -   **Activity Files**: Text files containing time logs (e.g., `activities.txt`).
    -   **Config**: TOML file defining quadrants and categories (`config.toml`).
    -   **CLI Args**: Runtime flags and file paths.

2.  **Parsing**:
    -   Reads lines from input files.
    -   Parses dates and activity lines into **Life Chunk** objects.
    -   Resolves categories to quadrants using the parsing state (initialized from configuration).
    -   Produces a stream of **Life Lapses**.

3.  **Merging**:
    -   Combines Life Lapses from multiple files.
    -   Sorts them by time.
    -   Merges adjacent/compatible blocks into continuous timelines.

4.  **Domain Logic**:
    -   **Life Lapse**: Represents a continuous block of time containing activities.
    -   **Life Chunk**: A single activity with duration, description, and categories.
    -   **Summary**: Aggregated statistics (duration per category/quadrant).

5.  **Output Generation**:
    -   **Plan**: Formats Life Lapses into a readable schedule.
    -   **Summary**: Computes statistics from Life Lapses and formats them.
    -   **Writing**: Writes formatted content to files or stdout.
    -   **Cleanup**: Removes redundant historical files before writing new ones.

## Key Modules

-   **Entry Point**: Handles CLI parsing and initiates the main loop.
-   **Application Logic**: Contains the core application loop and watch mode logic.
-   **Parsing**: Core parsing logic for the input format.
-   **Configuration**: Configuration loading and management.
-   **Domain**: Core data structures (Life Lapse, Life Chunk, etc.).
-   **Summary**: Logic for aggregating time and calculating statistics.
-   **Output**: Handles file I/O and writer management.
-   **Formatting**: Formats data for display.

## Design Principles

-   **Immutability**: Core domain objects are largely immutable after creation.
-   **Separation of Concerns**: Parsing, logic, and output are separated.
-   **Idempotency**: The output generation is deterministic based on the input.
-   **History Preservation**: The file cleanup strategy ensures that history is preserved only when it diverges, otherwise keeping the directory clean.
