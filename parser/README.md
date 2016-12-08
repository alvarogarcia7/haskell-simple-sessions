# Parser session

## Problem description

  * Create a binary expression calculator that takes a string and returns a boolean

## Versions

### Version `simple`

First version, using an unified parser and evaluator

You can find the code in the production code, under the function `calculate`. Also see the tests.

The work for this version is in the branch `parser/simple` ([Link](https://github.com/alvarogarcia7/haskell-simple-sessions/tree/parser/simple/parser))

### Version `with_own_types`

This version splits the parsing (lexing and parsing) and the evaluation

You can find the code in the production code, under the functions `parse` and `apply`. Also see the tests.

The work for this version is in a branch called `parser/with_own_types` ([Link](https://github.com/alvarogarcia7/haskell-simple-sessions/tree/parser/with_own_types/parser))

### Version `school_of_haskell_project` (WIP)

This version follows the [School of Haskell project][sohp], by Bartosz Milewski.

You can find the code in the production code, also see the tests.

### Running the tests

```
runhaskell -isrc -itest test/HSpecTests.hs
```

or 

```
make test
```

[sohp]: https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell
