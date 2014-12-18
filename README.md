Roogle
======

Search Engine for Ruby methods by approximate type signature

## How to use
~~~
  $ git clone https://github.com/Azabuhs/Roogle.git
  $ cd Roogle
  $ cabal sandbox init
  $ cabal install --only-dependencies
  $ cabal build
  $ ./dist/build/Roogle/roogle typesig.rb(or path to ruby program that is using Rubype to define type-signature)
~~~
## Test
~~~
  $ git clone https://github.com/Azabuhs/Roogle.git
  $ cd Roogle
  $ cabal sandbox init
  $ cabal install --enable-tests
  $ cabal repl tests
  then call the main function defined in Tests/Test.hs in the REPL
~~~
