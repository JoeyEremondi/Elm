Learn about the Elm programming language at [elm-lang.org](http://elm-lang.org/).

    
## Install
Haskelm is a modification on Elm. Therefore, it is reccomended that you install it in a Cabal sanbox.
To install the modified library, compiler, and haskelm binary, run

    cabal install

in the directory where you have checkoud the source
You can install only the haskelm executable with 
    cabal install haskelm

## Use

To use `haskelm` as a binary, simply run

    haskelm [infile]
Note that infile must contain Haskell declarations, but not imports,
module declarations, etc. (This should change in the near future)

The haskelm binary will print to stdout an Elm translation of the given haskell file.

## Library
You can also use Haskelm within a Haskell program, via Template Haskell.

Currently, you feed TemplateHaskell a list of haskell declarations.
It will then translate them into an Elm string, which is declared as a variable.
Then both the Haskell declarations and the Elm string will be spliced in.

To splice in a quoted list of declarations, use

    $(decHaskAndElm "variableNameForElmString" [d| ... |])

where your Haskell declarations go instide the `[d| ... |]` brackets.

Similarly, you can splice and translate declarations from a String or a file
using 

    $(decsFromString "elmFromString" "data X = Y | Z" )
or
    $(decsFromFile "elmFromFile" "input_file.hs" )

See tests/Tests/TH.hs for an example.

## Disclaimer

This is VERY much a work in progress, and is not production ready.
Please feel free to open issues for any bugs or feature suggestions.