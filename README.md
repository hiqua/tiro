# Tiro

**N.Bn.**: this code was written mostly to become familiar with Rust, and as
such is not idiomatic / clean etc.

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


## Random ideas (in)

* category tree
* define a category spreading over multiple activities to come (BEGIN / END)
* use quickcheck for testing
* profile program with a lot of data and see differences when copying or not
* use only one continuous file with all the days
