# File Cleanup Strategy

## Problem
`tiro` generates output files (plan, summary, global_summary) with high frequency, each stamped with a unique time. This leads to a proliferation of files in the output directory (e.g., `history/YYYY-wWW/YYYY-MM-DD/`). Many of these files are redundant because they represent intermediate states that are fully contained within subsequent files.

## Goal
Automatically remove redundant files to keep the output directory clean, preserving only the most relevant and complete information.

## Definition of Redundancy
An existing file is considered **redundant** if its entire content is a **prefix** of the new content about to be written.

- If `NewContent` starts with `OldContent`, then `OldContent` is redundant.
- This implies `OldContent` contains a subset of the information in `NewContent` (or exactly the same), in the same order.

## Observable Behavior

When `tiro` writes a new output file, it must perform a cleanup of the target directory **before** or **during** the write process.

1.  **Scope Identification**: The cleanup applies only to files in the same directory and of the same type (same prefix, e.g., `plan_`, `summary_`) as the file being written.
2.  **Redundancy Check**:
    - For every existing file in the scope:
        - If the content of the new file starts with the content of the existing file, the existing file is deleted.
3.  **File Creation**: The new file is created with the current timestamp.

## Edge Cases

### Identical Content
If the new content is exactly the same as an existing file's content:
- The existing file is a prefix of the new file (and vice versa).
- The existing file is deleted.
- The new file is created.
- **Result**: The file is effectively "touched" to the new timestamp.

### Non-Prefix Content (History Preservation)
If the user modifies an earlier activity or deletes lines, the new content will likely **not** start with the old content.
- The redundancy condition is **False**.
- The old file is **kept**.
- **Result**: Both files exist, preserving the history of the change.
