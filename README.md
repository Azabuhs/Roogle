Roogle
======

Search Engine for Ruby methods by approximate type signature

## How to use
~~~
  $ cabal sandbox init
  $ cabal install --only-dependencies
  $ cabal build

  $ ./dist/build/roogle typesig.rb(or path to ruby program that is using Rubype to define type-signature)
  $ cabal repl tests
~~~
## Test
~~~
  $ git clone https://github.com/Azabuhs/Roogle.git
  $ cd Roogle
  $ cabal sandbox init
  $ cabal install --enable-tests
  $ cabal repl tests
  $ all main function defined in Tests/Test.hs in the REPL
~~~
