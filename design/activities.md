# `activities.txt` Format Specification

The `activities.txt` file is used to log activities and time usage for `tiro`. The file is processed sequentially.

## General Structure

- The file consists of a sequence of **Time Blocks**.
- Each block begins with a **Date Line** that sets the current timestamp.
- Following the Date Line are one or more **Activity Lines**.
- **Comments**: Lines starting with `#` are ignored.
- **Empty Lines**: Ignored.

## Date Line

Sets the starting date and time for the subsequent activities.

**Format:**
- `YYYY-MM-DD HH:MM` (e.g., `2023-10-27 14:30`)
- `YYYY-MM-DD HHhMM` (e.g., `2023-10-27 14h30`)
- RFC3339 format is also supported (e.g., `2023-10-27T14:30:00+00:00`).

## Activity Line

Describes a single activity with a duration.

**Format:**
```text
HOURS MINUTES DESCRIPTION [TAGS...]
```

**Components:**
1.  **HOURS** (Required): Integer. The number of hours the activity took.
2.  **MINUTES** (Required): Integer. The number of minutes the activity took.
    *   **Important**: The first two tokens of the line are *always* consumed as duration. If they are not numbers, they are treated as 0 duration but are still removed from the description.
    *   *Example*: `Meeting with team` will result in 0 duration and the description `team` (since "Meeting" and "with" are consumed as invalid duration numbers).
    *   *Correct Usage*: `1 30 Meeting with team` (1 hour 30 mins).
    *   *Correct Usage (0 duration)*: `0 0 Meeting with team`.
3.  **DESCRIPTION**: The text describing the activity. Includes all tokens that are not duration numbers and not tags.
4.  **TAGS**: Optional tokens starting with `@`.
    *   **Quadrants**: `@1`, `@2`, `@3`, `@4`, `@5`, `@6`.
        *   Explicitly assigns the activity to a specific quadrant (Q1-Q6).
    *   **Categories**: Any other token starting with `@` (e.g., `@work`, `@study`, `@admin`).
        *   Categories can be mapped to quadrants in the `config.toml` file.
        *   If a category is not mapped, it defaults to a regular category.

## Example

```text
# Start of a new day
2023-10-27 09:00

# 30 minutes of planning (Q1)
0 30 Daily Planning @1

# 2 hours of coding (Category @work, mapped to Q1 or Q2 in config)
2 0 Implement feature X @work

# 15 minutes break (Category @break)
0 15 Coffee break @break

# 1 hour meeting (Implicitly 0 hours, 60 minutes is also valid)
0 60 Team meeting @work

# Reset time for evening
2023-10-27 18:00
1 30 Gym @health @2
```

## Parsing Rules Summary

1.  **Tokenization**: Lines are split by whitespace or commas.
2.  **Duration Parsing**:
    - The **first token** is parsed as Hours.
    - The **second token** is parsed as Minutes.
    - If parsing fails for either, the value is 0, but the token is consumed.
3.  **Tag Parsing**:
    - Tokens **must start with `@`** to be considered tags.
    - `@1`-`@6` are **Quadrants**.
    - Any other token starting with `@` (e.g., `@work`, `@Q1`) is a **Category**.
        - Note: `@Q1` is a *Category*, not a Quadrant tag. It must be mapped in `config.toml` to function as a quadrant.
    - Tokens starting with `@@` (e.g., `@@tag`) or not starting with `@` (e.g., `Q1`) are treated as **Description**.
    - Tags are removed from the Description.
4.  **Description Construction**:
    - All remaining tokens are joined with spaces to form the Description.
    - **Empty Tokens**: If multiple delimiters are used (e.g., `1,0,Desc,,More`), empty tokens are preserved, resulting in extra spaces in the description (e.g., `Desc  More`).

## Multiple Files

When multiple `activities.txt` files are provided to `tiro`:

1.  **Independent Parsing**: Each file is parsed independently into time blocks ("Life Lapses").
2.  **Global Sorting**: All time blocks from all files are collected and sorted by their start time.
3.  **Merging**:
    - Time blocks that are strictly adjacent (the end of one block is exactly the start of the next) are merged into a single continuous block.
    - Gaps between blocks result in separate blocks.
    - The order in which files are passed to the binary does not matter, as the content is sorted by date.

