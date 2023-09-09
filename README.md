# Tiro

**N.B.**: this code was written mostly to become familiar with Rust, and as
such is not idiomatic / clean etc.

## Installation

```
cargo install tiro --locked
```

## Example

```
tiro -a example/activities.txt
```

## Why?

* reverse-planning
    * I need 1h12 to do X, 3h47 to do Y, do I have time to do Z after?

* same rationale as accounting, but with time


## Some design decisions

* Parse line by line.

* First symbols of the line should determine what the line is
    * Easier and faster, (to parse, but also to edit)

## (Non-Cargo) Requirements
* libdbus


## Future Features (Ideas)
### Ability to merge several plannings

* check if compatible
    * intervals are from starting date to end date (of last activity of
        segment)
        * could define an end-date syntax to specify manually, and insert it at
            the parsing stage
    * non-intersecting intervals
* merge if compatible
    * first option: insert smaller plan into bigger one
        * take parsed data, input line directly rather than using the file
        * need to register line when parsing, to be able to insert stuff back,
            right after the date
    * second option: keep two files separated, just generate one big plan
        * simply merge the parsed data
        * also related to hierachical categories (e.g., @work)
* ability to merge plannings if somehow compatible (comparison of times). Use dates with nothing in between as a marker, if start/end of activities match, then merge into the file

## Still open

### Input format
* could I just use toml or another markup language as input instead of custom?
* handle timestamps with format 015 0h 15min for faster input
* handle timestamps with only hours given

### TODO
* allow to add some activities without any starting time, e.g. to take into account 20min of work done in the morning without specifics
* write tests in every file
* sanity check on date of day: if distance of activities with current date is more than 20h, something fishy, warn about it
* option to hide categories from a subcategory, e.g. work
* option to output only categories (@work)
* try to use fzf to get autocompletion for tiro files
* suggest to switch to categories already known, if close
* output the width of screen somewhere, to adapt tmux
* show time left between lifelapses
* transition between activities
* watch config file as well
* also show categories in summary
* remove case sensitivity for categories and association categories to quadrant
* also save activity file
* multiline description
* detect if several quadrant for one category
* make colors configurable
* figure out why I need openssl-dev and if I can get rid of it
* ignore non existing input files
* use anyhow for errors instead of using custom errors

### Ideas (in)
* category tree depending on order of categories
* define a category spreading over multiple activities to come (BEGIN / END)
* use quickcheck for testing
* profile program with a lot of data and see differences when copying or not
* use only one continuous file with all the days
* need to indicate how long the activities are relative to each other
* @today to specify current date, but how to deal with modifications on the day after?
* add coefficient to length of task
* handle 'until when' time as input instead of duration
* branching in activities (two possible choices)
* deal with time shift correctly
