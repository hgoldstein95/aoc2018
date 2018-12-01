# Advent of Code 2018

## Dependencies
Make sure you have `stack` installed with the latest version of `ghc`.

## Running code for a given day
Simply run:
```
stack run <day>
```

## Architecture
Each day has a `Day<day>.hs` file as well as a `Day<day>` directory. The `run`
function in `Day<day>.hs` is the only thing that `Main.hs` will look at.
Supporting code can be put in the corresponding directory and imported using the
statement
```
import Day<day>.<NewFile>
```
