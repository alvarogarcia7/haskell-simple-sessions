## Parser session

Create a binary expression calculator that takes a string and returns a boolean

### Version (master)

First version, using an unified parser and evaluator

You can find the code in the production code, under the function `calculate`. 

### Version `parsing_with_own_types`

This version splits the parsing (lexing and parsing) and the evaluation

You can find the code in the production code, under the functions `parse` and `apply`.


### Running the tests

```
runhaskell -isrc -itest test/HSpecTests.hs
```

or 

```
make test
```
