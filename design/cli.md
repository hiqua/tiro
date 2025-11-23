# CLI Documentation

`tiro` is a command-line tool for tracking time and activities. It processes activity logs (text files) and generates summaries and plans.

## Usage

```bash
tiro [FLAGS] [OPTIONS] --config <FILE>
```

## Arguments

### Required

-   **`-c, --config <FILE>`**
    -   Sets the path to the configuration file (`config.toml`).
    -   **Example**: `tiro -c config.toml`

### Options

-   **`-a, --activities <FILE>...`**
    -   Sets the activity file(s) to use.
    -   Can be specified multiple times to merge multiple files.
    -   Use `-` to read from stdin.
    -   **Example**: `tiro -c config.toml -a activities.txt -a extra_activities.txt`

-   **`-p, --plan <FILE>`**
    -   Sets the output file for the **Plan** (detailed list of activities).
    -   If not specified, defaults to stdout (unless `--quiet` is used).
    -   **Example**: `tiro -c config.toml -p plan.txt`

-   **`-s, --summary <FILE>`**
    -   Sets the output file for the **Summary** (aggregated time per category/quadrant).
    -   If not specified, defaults to stdout (unless `--quiet` is used).
    -   **Example**: `tiro -c config.toml -s summary.txt`

### Flags

-   **`-w, --watch`**
    -   Enables **Watch Mode**. The program will monitor the input activity files for changes and re-run the processing automatically.
    -   Useful for keeping a terminal window open with the current plan/summary.

-   **`-n, --notify`**
    -   Enables **Notifications**.

-   **`-q, --quiet`**
    -   **Quiet Mode**. Suppresses standard output.
    -   Useful when running in the background or when only file output is desired.

## Environment Variables

-   **`$VAR` expansion in paths**:
    -   Paths in `config.toml` (e.g., for `activity_paths`) support environment variable substitution (e.g., `$HOME/activities.txt`).
