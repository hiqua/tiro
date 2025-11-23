# Output Documentation

`tiro` generates several types of output files to visualize and summarize the tracked activities.

## Output Types

### 1. Plan
-   **Description**: A detailed, chronological list of all activities (Life Lapses).
-   **Content**:
    -   Date headers (e.g., `## 2023-10-27`).
    -   Activity lines with start time, end time, duration, and description.
    -   **Format**: `HH:MM - HH:MM (Duration) Description`
-   **Usage**: Useful for seeing the exact schedule of the day.

### 2. Summary
-   **Description**: An aggregated view of time spent per category and quadrant for the *current context* (usually the current day or the specific file being processed).
-   **Content**:
    -   Total duration for the period.
    -   Breakdown by Quadrant (Q1-Q6).
    -   Breakdown by Category (e.g., `@work`, `@sleep`).
-   **Usage**: Quick overview of where time went for a specific day/file.

### 3. Global Summary
-   **Description**: An aggregated view of time spent across *all* input files provided to the CLI.
-   **Content**:
    -   Similar structure to the standard Summary but aggregates data from the beginning of the input history.
-   **Usage**: Long-term analysis of time usage.

## File Naming & Location

When output files are written to disk (via `-p` or `-s` flags), they follow a specific naming convention to ensure uniqueness and history preservation.

-   **Timestamping**: Files are typically suffixed or stamped with the current execution time (e.g., `2023-10-27T14_30_00...`).
-   **Directory**: Files are saved to the paths specified in the CLI arguments or config.

## File Cleanup Strategy

To prevent an explosion of redundant files in the output directory, `tiro` implements a **Prefix-Based Cleanup Strategy**.

### The Problem
Frequent runs of `tiro` (especially in watch mode) generate many files. Often, a new file is just an extension of the previous one (e.g., adding a new activity to the end of the day). Keeping all intermediate versions is unnecessary clutter.

### The Solution
Before writing a new file, `tiro` checks existing files in the target directory.

1.  **Redundancy Check**: An existing file is considered **redundant** if its *entire content* is a **prefix** of the new content about to be written.
    -   *Example*:
        -   Old File: `A, B, C`
        -   New File: `A, B, C, D`
        -   Result: Old File is redundant (it's just a prefix of the new one).
2.  **Action**: Redundant files are **deleted**.
3.  **Preservation**: If the history changed (e.g., an old activity was modified), the new file will *not* have the old file as a prefix. In this case, the old file is **kept**, preserving the history of the change.

This ensures that the directory contains only the "latest" versions of any given history branch, keeping it clean without losing data on retroactive changes.
