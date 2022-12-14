[![CI](https://github.com/alessandrocandolini/haskell-trees/actions/workflows/ci.yml/badge.svg)](https://github.com/alessandrocandolini/haskell-trees/actions/workflows/ci.yml) [![codecov](https://codecov.io/gh/alessandrocandolini/haskell-trees/branch/main/graph/badge.svg?token=PmvCqUrxgr)](https://codecov.io/gh/alessandrocandolini/haskell-trees)

# haskell-trees

An exploraration of purely functional representation and manipulation of tree-like data structures using Haskell, including:
* equivalent representations
* non-equivalent representations
* modified preorder tree traversal 
* traversing a tree using the State monad
* what recursion schemes can do for us when working with trees
* and more

## How to build and run locally

The project uses the [Haskell tool stack](https://docs.haskellstack.org/en/stable/README/).

Assuming `stack` is installed in the system, the project can be build by running
```
stack build
```
To build and also run the tests, run
```
stack test
```
which is equivalent to
```
stack build --test
```
To run with test coverage
```
stack test --coverage
```
which generates a textual and HTML report.

To run the executable,
```
stack exec haskell-trees-exe
```
For faster feedback loop,
```
stack test --fast --file-watch
```
To run `ghci` (with a version compatible with the resolver) run
```
stack ghci
```
For more information, refer to the `stack` official docs.
